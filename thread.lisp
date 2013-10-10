#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defclass forum-thread (meta-thread)
  ((title :initarg :title :accessor title))
  (:documentation "A thread in a forum category."))

(defclass forum-post (meta-post)
  ()
  (:documentation "A post in a general forum thread."))

(defmethod get-posts ((thread meta-thread) &key (start 0) (num -1))
  "Retrieve all or a subset of posts in a forum thread."
  (let ((page (format NIL "/threads/~a/" (id thread))))
    ($ (initialize (token-request page NIL) :type :HTML))
    (when (search "Error" ($ "h1" (text) (node)))
      (error 'forum-error :code 404 :page page :info ($ "label.OverlayCloser" (text) (node))))
    (crawl-nodes ".messageList>li" #'(lambda (node) (make-meta-post node thread 'forum-post))
      :start start :num num)))

(defmethod post ((thread meta-thread) message &key title)
  "Post a new message to a forum thread."
  NIL)
