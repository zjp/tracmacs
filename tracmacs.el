;;; tracmacs.el --- Interact with the Trac ticketing system -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zach Pearson

;; Author: Zach Pearson <zach@zjp.codes>
;; Keywords: tools

;;; Commentary:

;; tracmacs serves to allow you to authenticate with a Trac instance, so other code
;; can send requests as if you were using a web browser. It is currently very limited.

;;; Code:

(defgroup tracmacs nil
  "'tracmacs' allows you to interact with the Trac ticket tracker"
  :group 'convenience)


(defcustom tracmacs-trac-host nil
  "The base URL for your Trac instance"
  :type 'string :group 'tracmacs)

(defcustom tracmacs-ticket-types-alist nil
  "Possible ticket types, e.g. 'bug', 'enhancement', 'task'"
  :type 'alist
  :group 'tracmacs)

(defcustom tracmacs-milestone-list nil
  "Possible milestones"
  :type 'list
  :group 'tracmacs)

(defcustom tracmacs-ticket-priorities-alist nil
  "Possible ticket priorities, e.g. `major`, `minor`, `blocker`"
  :type 'alist
  :group 'tracmacs)

(defcustom tracmacs-ticket-resolutions-alist nil
  "Possible ticket resolutions, e.g. `wontfix`, `can\'t reproduce`, `worksforme`"
  :type 'alist
  :group 'tracmacs)

(defun tracmacs-authenticate ()
  "Authenticate to a Trac instance. url handles storing cookies"
  (kill-buffer (url-retrieve-synchronously (string-join `(,tracmacs-trac-host "login") "/"))))

(defun tracmacs-url-for-ticket (ticket-number)
  (string-join `(,tracmacs-trac-host "ticket" ,(number-to-string ticket-number)) "/"))

(defun tracmacs-get-ticket (ticket-number)
  "Retrieve the webpage of a Trac ticket"
  (url-retrieve-synchronously (tracmacs-url-for-ticket ticket-number)))

(defun tracmacs--get-ticket-modify-time (ticket-number)
  "Get the last time the ticket was modified, which must be included in requests
to Trac. This will set the trac_form_ticket cookie as a side effect."
  (if (null (tracmacs--get-auth-token))
      (tracmacs-authenticate))
  (let* ((html-dom-tree (with-current-buffer
                            (tracmacs-get-ticket ticket-number)
                          (libxml-parse-html-region (point-min) (point-max))))
         retval)
    (dolist (el (dom-by-tag (dom-by-class (dom-by-id html-dom-tree "propertyform") "buttons") 'input))
      (if (string= (dom-attr el 'name) "start_time")
          (setq retval (string-to-number (dom-attr el 'value)))))
    retval))

(defun tracmacs--url-cookie-retrieve (&optional secure)
  "*extremely exasperated sigh* because url-cookie-retrieve uses a regular
expression for string searching, simply calling (url-retrieve-host host) will, to
your utter astonishment also, not return the cookies listed under `host` in their
hash table of cookies"
  (let ((storage (if secure
                     (append url-cookie-secure-storage url-cookie-storage)
                   url-cookie-storage))
        (case-fold-search t)
        (host (url-host (url-generic-parse-url tracmacs-trac-host)))
        (localpart (nth 0 (url-path-and-query (url-generic-parse-url tracmacs-trac-host))))
        cookies retval localpart-match)
    (dolist (cur storage)
      (setq cookies (cdr cur))
      (if (and (car cur)
               (string= (car cur) host))
          ;; The domains match - a possible hit!
          (dolist (cur cookies)
            (and (if (and (stringp
                           (setq localpart-match (url-cookie-localpart cur)))
                          (stringp localpart))
                     (string-match (concat "^" (regexp-quote localpart-match))
                                   localpart)
                   (equal localpart localpart-match))
                 (not (url-cookie-expired-p cur))
                 (setq retval (cons cur retval))))))
    retval))

(defun tracmacs--get-form-token ()
  (let ((cookies (tracmacs--url-cookie-retrieve)) retval)
    (dolist (cur cookies)
      (if (string= (url-cookie-name cur) "trac_form_token")
          (setq retval (url-cookie-value cur))))
    retval))

(defun tracmacs--get-auth-token ()
  (let ((cookies (tracmacs--url-cookie-retrieve)) retval)
    (dolist (cur cookies)
      (if (string= (url-cookie-name cur) "trac_auth")
          (setq retval (url-cookie-value cur))))
    retval))

;; From https://www.emacswiki.org/emacs/UrlPackage
(defun tracmacs--format-request-data (args)
  (mapconcat (lambda (arg)
               (concat (url-hexify-string (car arg))
                       "="
                       (url-hexify-string (cdr arg))))
             args
             "&"))

(defun tracmacs-reopen-ticket (ticket-number &optional comment)
  (if (null (tracmacs--get-auth-token))
      (tracmacs-authenticate))
  (let* ((modify-time (number-to-string (tracmacs--get-ticket-modify-time ticket-number)))
         (form-token (tracmacs--get-form-token))
         (auth-token (tracmacs--get-auth-token))
         (trac-cookie-string (concat "trac_auth_token=" form-token "; trac_auth=" auth-token ";"))
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded") ("Cookies" . ,trac-cookie-string)))
         (url-request-data
          (tracmacs--format-request-data
           `(("__FORM_TOKEN" . ,form-token)
             ("comment" . ,comment)
             ("start_time" . ,modify-time)
             ("view_time" . ,modify-time)
             ("action" . "reopen")
             ("submit" . "Submit changes")))))
    (kill-buffer (url-retrieve-synchronously (tracmacs-url-for-ticket ticket-number)))))

(defun tracmacs-close-ticket (ticket-number &optional resolution comment)
  (if (null (tracmacs--get-auth-token))
      (tracmacs-authenticate))
  (let* ((modify-time (number-to-string (tracmacs--get-ticket-modify-time ticket-number)))
         (form-token (tracmacs--get-form-token))
         (auth-token (tracmacs--get-auth-token))
         (trac-cookie-string (concat "trac_auth_token=" form-token "; trac_auth=" auth-token ";"))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")
            ("Cookies" . ,trac-cookie-string)))
         (url-request-data
          (tracmacs--format-request-data
           `(("__FORM_TOKEN" . ,form-token)
             ("start_time" . ,modify-time)
             ("view_time" . ,modify-time)
             ("action" . "resolve")
             ("action_resolve_resolve_resolution" . ,resolution)
             ("comment" . ,comment)
             ("submit" . "Submit changes")))))
    (kill-buffer (url-retrieve-synchronously (tracmacs-url-for-ticket ticket-number)))))

(defun tracmacs-comment-on-ticket (ticket-number comment)
  ;; TODO: Should this be a hook so the user can choose whether or not to authenticate?
  (if (null (tracmacs--get-auth-token))
      (tracmacs-authenticate))
  (let* ((modify-time (number-to-string (tracmacs--get-ticket-modify-time ticket-number)))
         (form-token (tracmacs--get-form-token))
         (auth-token (tracmacs--get-auth-token))
         (trac-cookie-string (concat "trac_auth_token=" form-token "; trac_auth=" auth-token ";"))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")
            ("Cookies" . ,trac-cookie-string)))
         (url-request-data
          (tracmacs--format-request-data
           `(("__FORM_TOKEN" . ,form-token)
             ("start_time" . ,modify-time)
             ("view_time" . ,modify-time)
             ("action" . "leave")
             ("comment" . ,comment)
             ("submit" . "Submit changes")))))
    (kill-buffer (url-retrieve-synchronously (tracmacs-url-for-ticket ticket-number)))))
