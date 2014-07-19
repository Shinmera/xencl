#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defclass meta-forum ()
  ((id :initarg :id :initform NIL :accessor id)
   (title :initarg :title :initform NIL :accessor title))
  (:documentation "Super-class for forum-like classes that contain threads."))

(defmethod print-object ((forum meta-forum) out)
  (print-unreadable-object (forum out :type T)
    (format out "~a (~a)" (title forum) (id forum))))

(defgeneric get-threads (object &key &allow-other-keys)
  (:documentation "Retrieve all threads in a meta-forum."))
(defgeneric start-thread (object message &key &allow-other-keys)
  (:documentation "Create a new thread in a meta-forum."))

(defclass meta-thread ()
  ((id :initarg :id :initform NIL :accessor id)
   (op :initarg :op :initform NIL :accessor op)
   (time :initarg :time :initform NIL :accessor post-time))
  (:documentation "Super-class for thread-like classes that contain posts."))

(defmethod print-object ((thread meta-thread) out)
  (print-unreadable-object (thread out :type T)
    (format out "~a (~a)" (id thread) (op thread))))

(defgeneric get-posts (object &key &allow-other-keys)
  (:documentation "Retrieve all posts in a thread or post-flow"))
(defgeneric post (object message &key &allow-other-keys)
  (:documentation "Post a new message to a thread or post-flow."))

(defclass meta-post ()
  ((id :initarg :id :initform NIL :accessor id)
   (thread :initarg :thread :initform NIL :accessor thread)
   (author :initarg :author :initform NIL :reader author)
   (message :initarg :message :initform NIL :accessor message)
   (time :initarg :time :initform NIL :accessor post-time))
  (:documentation "Super-class for post-like classes."))

(defmethod print-object ((post meta-post) out)
  (print-unreadable-object (post out :type T)
    (format out "~a/~a (~a)" (id (thread post)) (id post) (author post))))

(defgeneric reply (object message &key &allow-other-keys)
  (:documentation "Reply to a thread or post-flow."))

(defgeneric report (post reason &key &allow-other-keys)
  (:documentation "Report a post to the moderators."))

(defgeneric edit (post new-message &key &allow-other-keys)
  (:documentation "Edit a post."))

(defgeneric delete-post (post &key &allow-other-keys)
  (:documentation "Delete a post."))

(defun make-meta-post (node &optional thread (class 'meta-post))
  (make-instance class
                 :id ($ node (attr :id) (node))
                 :thread thread
                 :author ($ node (attr :data-author) (node))
                 :message ($ node "blockquote.messageText" (html) (node))
                 :time (parse-post-datetime ($ node ".DateTime" (attr :title) (node)))))
