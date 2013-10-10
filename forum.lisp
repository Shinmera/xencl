#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defclass forum (meta-forum)
  ()
  (:documentation "Representation of a forum that contains threads."))

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
