#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defvar *cookies* (make-instance 'drakma:cookie-jar))
(defvar *token*)
(defvar *index*)
(defvar *user*)

(define-condition forum-error (error)
  ((info :initarg :info)
   (code :initarg :code))
  (:documentation "Condition signalled when a forum request returns an error."))

(defmethod print-object ((obj forum-error) stream)
  (format stream "~a (~a)" (slot-value obj 'info) (slot-value obj 'code)))

(defun url (url) (concatenate 'string *index* url))

(defun request (url &optional extraparams stream)
  "Short wrapper around drakma to make request statements slimmer."
  (restart-case
      (let ((resp (drakma:http-request (url url)
                                       :method :POST :parameters extraparams
                                       :cookie-jar *cookies*
                                       :want-stream stream
                                       :external-format-out :UTF-8)))
        (log:trace "Reply to ~a with ~a: ~a" url extraparams resp)
        resp)
    (re-request (&optional new-extraparams)
      (request url (or new-extraparams extraparams)))))

(defun get-text (selector)
  (string-trim '(#\Newline #\Space #\Tab) 
               (format NIL "~{~a~}" 
                       ($ selector (contents) 
                          (each #'(lambda (node) (if (dom:text-node-p node) (dom:data node) "")) :replace T)))))

(defun token (&optional page)
  (if page (request page))
  (let ((token ($ "input[name='_xfToken']" (attr :value) (node))))
    (assert (and token (> (length token) 0)) ()
            'forum-error :code 3 :info "Token empty or not found on page!")
    (setf *token* token)))

(defun token-request (url params &key (forum-token *token*) stream)
  (assert (not (eq forum-token NIL)) (forum-token) "Forum token is not available! Are you successfully logged in?")
  (log:debug "Forum token request ~a: ~a" url params)
  (request url (acons "_xfToken" forum-token params) stream))

(defun initiate (index user pass)
  (setf *index* index)
  (login (make-instance 'user :id user :pass pass)))
