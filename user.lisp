#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defclass user (meta-forum)
  ((pass :initarg :pass :reader pass))
  (:documentation "Standard user object for user related interactions."))

(defclass profile-thread (meta-thread)
  ()
  (:documentation "Thread on the profile page of a user."))

(defclass profile-post (meta-post)
  ()
  (:documentation "Post on a profile-thread of a user."))

(defgeneric login (user) (:documentation "Attempt to log in with the provided user's credentials (id & pass)"))

(defmethod login ((user user))
  (setf *cookies* (make-instance 'drakma:cookie-jar)
        *user* user)
  ($ (initialize
      (request "/login") :type :HTML))
  (assert (string-equal (url "/") ($ "head base" (attr :href) (node))) ()
          'forum-error :code 1 :info "Header base does not match index page!")
  ($ (initialize
      (request "/login/login"
                     `(("login" . ,(title user))
                       ("password" . ,(pass user))
                       ("register" . "0")
                       ("remember" . "1")
                       ("cookie_check" . "1")
                       ("redirect" . ,(url "/"))
                       ("_xfToken" . ""))) :type :HTML))
  (assert (not (search "Error" ($ "h1" (text) (node)))) ()
          'forum-error :code 2 :info (format NIL "Error while logging in: ~a" (get-text ".pageContent")))
  (token))

(defgeneric logout (user) (:documentation "Log the currently logged in user out again."))

(defmethod logout ((user user))
  (token-request "/logout/" NIL)
  (setf *token* NIL *cookies* NIL))

(defun get-user-id (user)
  "Retrieve the actual ID of a user."
  ; TODO
  )

(defmethod start-thread ((user user) message &key)
  "Start a new thread on the profile page of a user."
  (checked-request (format NIL "/members/~a/post" (id user))
                   `(("message" . ,message)))
  ; GET THREAD INSTANCE!
  )

(defmethod get-threads ((user user) &key (start 0) (num 20))
  "Retrieve the threads on the profile page of a user."
  (flet ((make-thread (node)
           (make-instance 'profile-thread
                          :id (let ((id ($ node (attr :id) (node))))
                                (subseq id (1+ (search "-" id :from-end T))))
                          :op ($ node (attr :data-author) (node))
                          :time (parse-post-datetime ($ node ".messageInfo .DateTime" (attr :title) (node))))))
    (checked-request (format NIL "/members/~a/" (id user)) NIL)
    (crawl-nodes "#ProfilePostList>li" #'make-thread :start start :num num)))

(defmethod get-posts ((thread profile-thread) &key (start 0) (num 20))
  "Retrieve posts from a profile thread."
  )

(defmethod post ((thread profile-thread) message &key)
  "Post a new message to a thread on a user profile."
  )

(defmethod reply ((post profile-post) message &key)
  "Reply to a post on a user-profile thread."
  (post (thread post) (format NIL "@~a: ~a" (author post) message)))
