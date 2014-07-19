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
  ((title :initarg :title :initform NIL :accessor title))
  (:documentation "A thread in a forum category."))

(defclass forum-post (meta-post)
  ()
  (:documentation "A post in a general forum thread."))

(defun get-forums ()
  "Retrieve all forums."
  (let ((*lquery-master-document*))
    (checked-request "/forum/" NIL)
    (loop for node across ($ "li.node.forum .nodeTitle a")
          collect (make-instance 'forum
                                 :id (let ((href ($ node (attr :href) (node))))
                                       (subseq href (1+ (search "/" href)) (search "/" href :from-end T)))
                                 :title ($ node (text) (node))))))

(defmethod get-threads ((forum forum) &key (start 0) (num 20))
  "Retrieve all or a subset of threads in a forum."
  (let ((*lquery-master-document*))
    (flet ((make-thread (node)
             (make-instance 'forum-thread
                            :id (let ((href ($ node ".titleText>.title>a" (node) (attr :href) (node))))
                                  (subseq href (1+ (search "/" href)) (search "/" href :from-end T)))
                            :title ($ node ".titleText>.title>a" (text) (node))
                            :op ($ node ".titleText .username" (text) (node))
                            :time (parse-post-datetime ($ node ".titleText .DateTime" (text) (node))))))
      (checked-request (format NIL "/forums/~a/" (id forum)) NIL)    
      (crawl-nodes ".discussionListItems>li" #'make-thread
                   :start start :num num))))

(defmethod start-thread ((forum forum) message &key title)
  "Start a new thread in a given forum."
  (let ((*lquery-master-document*))
    (checked-request (concatenate 'string "/forums/" (id forum) "/add-thread")
                     `(("message_html" . ,message)
                       ("title" . ,title)))
    (make-instance 'forum-thread
                   :id (let ((id ($ "a[title=\"Permalink\"]" (attr :href) (node))))
                         (subseq id (1+ (search "/" id)) (search "/" id :from-end T)))
                   :title title
                   :op (title *user*)
                   :time (parse-post-datetime ($ "a[title=\"Permalink\"] abbr.DateTime" (text) (node))))))

(defmethod get-posts ((thread forum-thread) &key (start 0) (num 20))
  "Retrieve all or a subset of posts in a forum thread."
  (let ((*lquery-master-document*))
    (checked-request (format NIL "/threads/~a/" (id thread)) NIL)
    (crawl-nodes ".messageList>li" #'(lambda (node) (make-meta-post node thread 'forum-post))
                 :start start :num num)))

(defmethod post ((thread forum-thread) message &key)
  "Post a new message to a forum thread."
  (let ((*lquery-master-document*))
    (checked-request (concatenate 'string "/threads/" (id thread) "/add-reply")
                     `(("message_html" . ,message)))
    (let ((post ($ ".messageList>li" (last))))
      (make-meta-post post thread 'forum-post))))

(defmethod reply ((post forum-post) message &key)
  "Reply to a given post."
  (let ((message (format NIL "[quote=\"~a, post: ~a\"]~a[/quote]~%~a"
                         (author post) (id post) (message post) message)))
    (post (thread post) message)))

(defgeneric rate (post rating) (:documentation "Pass a rating for a given post. The rating ID is forum-dependant."))

(defmethod rate ((post forum-post) rating)
  (token-request (format NIL "/posts/~a/rate" (id post))
                 `(("rating" . ,rating))))

(defmethod report ((post forum-post) reason &key)
  )

(defmethod edit ((post forum-post) new-message &key)
  )

(defmethod delete-post ((post forum-post) &key)
  )
