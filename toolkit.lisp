#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defvar *cookies* (make-instance 'drakma:cookie-jar) "Contains the cookies required for authentication.")
(defvar *token* NIL "Contains the security token required in almost all requests.")
(defvar *index* NIL "String for the index of the forum without trailing slash.")
(defvar *user* NIL "Object containing the currently logged in user.")

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

(defun checked-request (url params &key (forum-token *token*) stream)
  ($ (initialize (token-request url params :forum-token forum-token :stream stream) :type :HTML))
  (when (search "Error" ($ "h1" (text) (node)))
    (error 'forum-error :code 404 :page url :info ($ "label.OverlayCloser" (text) (node)))))

(defun initiate (index user pass)
  "Shorthand function to set up XenCL."
  (setf *index* index)
  (login (make-instance 'user :title user :pass pass)))

(defmacro do-pageinated (predicate &body body)
  (let ((funcsym (gensym "FUNC"))
        (navsym (gensym "NAV"))
        (rootsym (gensym "ROOT"))
        (itersym (gensym "ITER")))
    `(flet ((,funcsym () ,@body))
       (,funcsym)
       (let ((,navsym ($ ".PageNav" (node))))
         (when ,navsym
           (loop with ,rootsym = (concatenate 'string "/" ($ ,navsym "a[rel=\"start\"]" (attr :href) (node)))
              for ,itersym from 2 upto (parse-integer ($ ,navsym (attr :data-last) (node)))
              while ,predicate
              do ($ (initialize (token-request ,rootsym `(("page" . ,(format NIL "~a" ,itersym)))) :type :HTML))
                (,funcsym)))))))

(defun crawl-nodes (node parser &key (start 0) (num -1))
  (flet ((parse-posts ()
           (loop for node in ($ node)
              collect (funcall parser node))))
    (let ((posts ())
          (end (+ start num)))
      (do-pageinated (or (< num 0) (<= (length posts) end))
        (setf posts (append posts (parse-posts))))
      (if (> (length posts) start)
          (if (or (< num 0) (<= (length posts) end))
              (subseq posts start)
              (subseq posts start end))
          NIL))))

(defun parse-post-integer (integer)
  (if (= 0 (length integer))
      -1
      (parse-integer (cl-ppcre:regex-replace-all "\\," integer ""))))

(defvar *monthnames-to-int*
  '(:JAN 1 :FEB 2 :MAR 3 :APR 4 :MAY 5 :JUN 6 :JUL 7 :AUG 8 :SEP 9 :OCT 10 :NOV 11 :DEC 12))

(defun parse-post-datetime (datetime)
  (if (= 0 (length datetime))
      NIL
      (flet ((parse-time (timestamp time)
               (let* ((pos (search " " time))
                      (ampm (subseq time (1+ pos)))
                      (time (subseq time 0 pos))
                      (pos (search ":" time))
                      (hours (parse-integer (subseq time 0 pos)))
                      (mins (parse-integer (subseq time (1+ pos))))
                      (hours (if (string= ampm "PM") (+ hours 12) hours)))
                 (local-time:adjust-timestamp! timestamp
                   (set :sec 0) (set :minute mins) (set :hour hours))))
             (parse-date (timestamp date)
               (let* ((pos (search "," date))
                      (year (parse-integer (subseq date (+ 2 pos))))
                      (date (subseq date 0 pos))
                      (pos (search " " date))
                      (day (parse-integer (subseq date (1+ pos))))
                      (month (string-upcase (subseq date 0 pos)))
                      (month (getf *monthnames-to-int* (find-symbol month "KEYWORD"))))
                 (local-time:adjust-timestamp! timestamp
                   (set :year year) (set :month month) (set :day-of-month day)))))
        (let ((pos (search "at" datetime))
              (timestamp (local-time:make-timestamp)))
          (if pos
              (progn (parse-date timestamp (subseq datetime 0 (1- pos)))
                     (parse-time timestamp (subseq datetime (+ pos 3))))
              (parse-date timestamp datetime))))))
