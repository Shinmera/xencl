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
   (code :initarg :code)
   (page :initarg :page))
  (:documentation "Condition signalled when a forum request returns an error."))

(defmethod print-object ((obj forum-error) stream)
  (format stream "~a (~a) ~a" (slot-value obj 'info) (slot-value obj 'code) (slot-value obj 'page)))

(defun url (url) (concatenate 'string *index* url))

(defun request (url &optional extraparams stream)
  "Short wrapper around drakma to make request statements slimmer."
  (restart-case
      (let ((resp (drakma:http-request (url url)
                                       :method :POST :parameters extraparams
                                       :cookie-jar *cookies*
                                       :want-stream stream
                                       :external-format-out :UTF-8)))
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
  (request url (acons "_xfToken" forum-token params) stream))

(defun initiate (index user pass)
  (setf *index* index)
  (login (make-instance 'user :id user :pass pass)))

(defun crawl-posts (&optional (post-parser #'make-meta-post))
  (flet ((parse-posts ()
           (loop for node in ($ ".messageList li")
              collect (funcall post-parser node))))
    (let ((posts (parse-posts))
          (nav ($ ".PageNav" (node))))
      (when nav
        (loop with root = ($ nav "a[rel=\"start\"]" (attr :href) (node))
           for i from 2 upto (parse-integer ($ nav (attr :data-last) (node)))
           do ($ (initialize (token-request (format NIL "/~apage-~a" root i) NIL) :type :HTML))
             (setf posts (append posts (parse-posts)))))
      posts)))

(defvar *monthnames-to-int*
  '(:JAN 1 :FEB 2 :MAR 3 :APR 4 :MAY 5 :JUN 6 :JUL 7 :AUG 8 :SEP 9 :OCT 10 :NOV 11 :DEC 12))

(defun parse-post-datetime (datetime)
  (if (= 0 (length datetime))
      NIL
      (let* ((pos (search "at" datetime))
             (date (subseq datetime 0 (1- pos)))
             (time (subseq datetime (+ pos 3)))
             
             (pos (search " " time))
             (ampm (subseq time (1+ pos)))
             (time (subseq time 0 pos))
             (pos (search ":" time))
             (hours (parse-integer (subseq time 0 pos)))
             (mins (parse-integer (subseq time (1+ pos))))
             (hours (if (string= ampm "PM") (+ hours 12) hours))
             
             (pos (search "," date))
             (year (parse-integer (subseq date (+ 2 pos))))
             (date (subseq date 0 pos))
             (pos (search " " date))
             (day (parse-integer (subseq date (1+ pos))))
             (month (string-upcase (subseq date 0 pos)))
             (month (getf *monthnames-to-int* (find-symbol month "KEYWORD")))

             (timestamp (local-time:make-timestamp)))

        (local-time:adjust-timestamp! timestamp
          (set :sec 0) (set :minute mins) (set :hour hours)
          (set :year year) (set :month month) (set :day-of-month day)))))
