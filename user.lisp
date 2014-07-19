#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defclass user (meta-forum)
  ((pass :initarg :pass :initform NIL :reader pass)
   (like-count :initarg :like-count :initform NIL :reader like-count)
   (message-count :initarg :message-count :initform NIL :reader message-count)
   (trophy-count :initarg :trophy-count :initform NIL :reader trophy-count)
   (follower-count :initarg :follower-count :initform NIL :reader follower-count)
   (register-date :initarg :register-date :initform NIL :reader register-date)
   (last-activity :initarg :last-activity :initform NIL :reader last-activity))
  (:documentation "Standard user object for user related interactions."))

(defmethod enumerate ((user user))
  (format T "ID: ~a~%~
             Title: ~a~%~
             Register-Date: ~a~%~
             Last-Activity: ~a~%~
             Messages: ~d~%~
             Tophies: ~d~%~
             Followers: ~d"
          (id user) (title user)
          (format-time (register-date user))
          (format-time (last-activity user))
          (message-count user) (trophy-count user) (follower-count user)))

(defclass profile-thread (meta-thread)
  ()
  (:documentation "Thread on the profile page of a user."))

(defclass profile-post (meta-post)
  ()
  (:documentation "Post on a profile-thread of a user."))

(defgeneric login (user) (:documentation "Attempt to log in with the provided user's credentials (id & pass)"))

(defmethod login ((user user))
  (let ((*lquery-master-document*))
    (setf *cookies* (make-instance 'drakma:cookie-jar)
          *user* user)
    ($ (initialize (request "/login")))
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
                   ("_xfToken" . "")))))
    (assert (not (search "Error" ($ "h1" (text) (node)))) ()
            'forum-error :code 2 :info (format NIL "Error while logging in: ~a" (get-text ".pageContent")))
    (token)))

(defgeneric logout (user) (:documentation "Log the currently logged in user out again."))

(defmethod logout ((user user))
  (let ((*lquery-master-document*))
    (token-request "/logout/" NIL)
    (setf *token* NIL *cookies* NIL)))

(defun get-user (username)
  ;; We save a page load by using that getting the user-id also gets us the profile page.
  ;; Yay for hacks.
  (let ((*lquery-master-document*))
    (let ((id (get-user-id username))
          (infos ($ ".profilePage .mast .secondaryContent" (first) "dd")))
      (make-instance 'user :id id :title username
                           :pass NIL
                           :last-activity (parse-post-datetime ($ infos (eq 0) (text) (node)))
                           :register-date (parse-post-datetime ($ infos (eq 1) (text) (node)))
                           :follower-count (parse-post-integer ($ ".followBlocks .section" (eq 1) ".count" (text) (node)))
                           :trophy-count (parse-post-integer ($ infos (eq 3) (text) (node)))
                           :message-count (parse-post-integer ($ infos (eq 2) (text) (node)))
                           :like-count (parse-post-integer ($ infos (eq 4) (text) (node)))))))

(defgeneric get-user-id (user) (:documentation "Retrieve the ID of a user from their title."))

(defmethod get-user-id ((user user))
  (get-user-id (title user)))

(defmethod get-user-id ((user string))
  (let ((*lquery-master-document*))
    (checked-request "/members/" `(("username" . ,user)))
    (let ((id ($ ".crumb" (last) (attr :href) (node))))
      (subseq id (1+ (search "/" id :from-end T :end2 (1- (length id)))) (search "/" id :from-end T)))))

(defmethod start-thread ((user user) message &key)
  "Start a new thread on the profile page of a user."
  (let ((*lquery-master-document*))
    (checked-request (format NIL "/members/~a/post" (id user))
                     `(("message" . ,message))))
  ; GET THREAD INSTANCE!
  )

(defmethod get-threads ((user user) &key (start 0) (num 20))
  "Retrieve the threads on the profile page of a user."
  (let ((*lquery-master-document*))
    (flet ((make-thread (node)
             (make-instance 'profile-thread
                            :id (let ((id ($ node (attr :id) (node))))
                                  (subseq id (1+ (search "-" id :from-end T))))
                            :op ($ node (attr :data-author) (node))
                            :time (parse-post-datetime ($ node ".messageInfo .DateTime" (attr :title) (node))))))
      (checked-request (format NIL "/members/~a/" (id user)) NIL)
      (crawl-nodes "#ProfilePostList>li" #'make-thread :start start :num num))))

(defmethod get-posts ((thread profile-thread) &key (start 0) (num 20))
  "Retrieve posts from a profile thread."
  )

(defmethod post ((thread profile-thread) message &key)
  "Post a new message to a thread on a user profile."
  )

(defmethod reply ((post profile-post) message &key)
  "Reply to a post on a user-profile thread."
  (post (thread post) (format NIL "@~a: ~a" (author post) message)))

(defun get-users (&key (start 0) (num 40) (order-by :USERNAME) (order-direction :ASC))
  "Get a list of users. Sorting requires this http://raid101.com/community/forums/xenforo-development.114/ plugin to be installed.
Order-by can be one of (:USERNAME :REGISTER-DATE :MESSAGE-COUNT :LIKE-COUNT :TROPHY-POINTS :FOLLOW-COUNT :LAST-ACTIVITY)
Order-direction can be one of (:ASC :DESC)"
  (let ((*lquery-master-document*))
    (flet ((make-user (node)
             (let ((infos ($ node ".userStats dd"))
                   (username ($ node "h3.username")))
               (make-instance 'user 
                              :id (let ((id ($ username "a" (attr :href) (node))))
                                    (subseq id (1+ (search "/" id)) (search "/" id :from-end T)))
                              :title ($ username "span" (text) (node))
                              :follow-count (parse-post-integer ($ infos (eq 3) (text) (node)))
                              :trophy-count (parse-post-integer ($ infos (eq 2) (text) (node)))
                              :message-count (parse-post-integer ($ infos (eq 0) (text) (node)))
                              :like-count (parse-post-integer ($ infos (eq 1) (text) (node)))))))
      (checked-request "/members/" `(("sort" . ,(cl-ppcre:regex-replace "-" (string-downcase order-by) "_"))
                                     ("dir" . ,(string-upcase order-direction))))
      (crawl-nodes ".memberList>li" #'make-user :start start :num num))))
