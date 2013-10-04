#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defclass conversation (meta-thread)
  ((id :initarg :id :accessor id)
   (op :initarg :op :accessor op)
   (time :initarg :time :accessor post-time)
   (title :initarg :title :accessor title))
  (:documentation "Object for interacting with user conversations."))

(defmethod print-object ((conversation conversation) out)
  (print-unreadable-object (conversation out :type T)
    (format out "~a: ~a" (op conversation) (id conversation))))

(defun to-participants (participants)
  (if (listp participants)
      (format NIL "~{~a~^, ~}" participants)
      participants))

(defmethod conversations ((user user))
  (flet ((make-conversation (node)
           (format T "~a" ($ node (serialize)))
           (make-instance 'conversation
                          :id (let ((id ($ node "h3.title a" (first) (attr :href) (node))))
                                (subseq id (1+ (search "/" id)) (1- (search "/" id :from-end T))))
                          :op ($ node (attr :data-author) (node))
                          :time (let ((datetime ($ node ".posterDate .DateTime" (attr :data-time) (node))))
                                  (if (> (length datetime) 0)
                                      (local-time:unix-to-timestamp (parse-integer datetime))))
                          :title ($ node "h3.title a" (first) (text) (node)))))
    ($ (initialize (token-request "/conversations" NIL) :type :HTML))
    ($ ".discussionListItems li"
       (each #'make-conversation :replace T))))

(defmethod start-conversation ((user user) participants title message)
  ($ (initialize (token-request "/conversations/insert"
                                `(("recipients" . ,(to-participants participants))
                                  ("title" . ,title)
                                  ("message_html" . ,message)))
                 :type :HTML))
  (make-instance 'conversation
                 :id (let ((id ($ ".pageNavLinkGroup .linkGroup a" (first) (attr :href) (node))))
                       (subseq id (1+ (search "/" id)) (search "/" id :from-end T)))
                 :op ($ ".message" (first) (attr :data-author) (node))
                 :time ($ ".message" (first) ".messageMeta .datePermalink" (attr :data-time) (node))
                 :title ($ ".titleBar h1" (text) (node))))

(defmethod get-posts ((conversation conversation) &key)
  ($ (initialize (token-request (concatenate 'string "/conversations/" (id conversation)) NIL) :type :HTML))
  ;TODO: (crawl-posts)
  )

(defmethod post ((conversation conversation) message &key)
  (token-request (concatenate 'string "/conversations/" (id conversation) "/insert-reply")
                 `(("message_html" . ,message))))

(defmethod invite ((conversation conversation) participants)
  (assert (string-equal (id *user*) (op conversation)) ()
          'forum-error :code 4 :text (format NIL "Cannot invite users as you are not the OP (~a != ~a)." (id *user*) (op conversation)))
  (token-request (concatenate 'string "/conversations/" (id conversation) "/invite-insert")
                 `(("recipients" . ,(to-participants participants)))))
