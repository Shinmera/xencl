#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defclass forum (meta-forum)
  ()
  (:documentation "Representation of a forum that contains threads."))

(defclass forum-thread (meta-thread)
  ((title :initarg :title :accessor title))
  (:documentation "A thread in a forum category."))

(defclass forum-post (meta-post)
  ()
  (:documentation "A post in a general forum thread."))

(defun get-forums ()
  "Retrieve all forums."
  ($ (initialize (token-request "/forum/" NIL) :type :HTML))
  (loop for node in ($ "li.node.forum .nodeTitle a")
     collect (make-instance 'forum
                            :id (let ((href ($ node (attr :href) (node))))
                                  (subseq href (1+ (search "/" href)) (search "/" href :from-end T)))
                            :title ($ node (text) (node)))))

(defmethod get-threads ((forum forum) &key (start 0) (num 20))
  "Retrieve all or a subset of threads in a forum."
  (flet ((make-thread (node)
           (make-instance 'forum-thread
                          :id (let ((href ($ node ".titleText>.title>a" (node) (attr :href) (node))))
                                (subseq href (1+ (search "/" href)) (search "/" href :from-end T)))
                          :title ($ node ".titleText>.title>a" (text) (node))
                          :op ($ node ".titleText .username" (text) (node))
                          :time (parse-post-datetime ($ node ".titleText .DateTime" (text) (node))))))
    (let ((page (format NIL "/forums/~a/" (id forum))))
      ($ (initialize (token-request page NIL) :type :HTML))
      (when (search "Error" ($ "h1" (text) (node)))
        (error 'forum-error :code 404 :page page :info ($ "label.OverlayCloser" (text) (node))))
      (crawl-nodes ".discussionListItems>li" #'make-thread
                   :start start :num num))))

(defmethod start-thread ((forum forum) message &key title)
  "Start a new thread in a given forum."
  (token-request (concatenate 'string "/forums/" (id forum) "/add-thread")
                 `(("message_html" . ,message)
                   ("title" . ,title)))
  ;GET THREAD INSTANCE!
  )

(defmethod get-posts ((thread forum-thread) &key (start 0) (num -1))
  "Retrieve all or a subset of posts in a forum thread."
  (let ((page (format NIL "/threads/~a/" (id thread))))
    ($ (initialize (token-request page NIL) :type :HTML))
    (when (search "Error" ($ "h1" (text) (node)))
      (error 'forum-error :code 404 :page page :info ($ "label.OverlayCloser" (text) (node))))
    (crawl-nodes ".messageList>li" #'(lambda (node) (make-meta-post node thread 'forum-post))
      :start start :num num)))

(defmethod post ((thread forum-thread) message &key)
  "Post a new message to a forum thread."
  (token-request (concatenate 'string "/threads/" (id thread) "/add-reply")
                 `(("message_html" . ,message)))
  ;GET POST INSTANCE!
  )

(defmethod reply ((post forum-post) message &key)
  "Reply to a given post."
  )
