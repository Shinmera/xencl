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

(defmethod get-threads ((forum forum) &key)
  ())
