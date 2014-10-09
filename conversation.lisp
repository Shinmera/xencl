#|
  This file is a part of XenCL
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defclass conversation (meta-thread)
  ()
  (:documentation "Object for interacting with user conversations."))

(defclass conversation-post (meta-post)
  ()
  (:documentation "A post in a private conversation thread."))

(defmethod print-object ((conversation conversation) out)
  (print-unreadable-object (conversation out :type T)
    (format out "~a: ~a" (op conversation) (id conversation))))

(defun to-participants (participants)
  (if (listp participants)
      (format NIL "~{~a~^, ~}" participants)
      participants))

(defgeneric conversations (user)
  (:documentation "Retrieve the conversations the user has participated in. Note that this only works for the logged in user."))

(defmethod conversations ((user user))
  (let ((*lquery-master-document*))
    (flet ((make-conversation (node)
             (make-instance 'conversation
                            :id (let ((id ($ node "h3.title a" (first) (attr :href) (node))))
                                  (subseq id (1+ (search "/" id)) (search "/" id :from-end T)))
                            :op ($ node (attr :data-author) (node))
                            :time (let ((datetime ($ node ".posterDate .DateTime" (attr :data-time) (node))))
                                    (if (> (length datetime) 0)
                                        (local-time:unix-to-timestamp (parse-integer datetime))))
                            :title ($ node "h3.title a" (first) (text) (node)))))
      (checked-request "/conversations" NIL)
      ($ ".discussionListItems li"
        (each #'make-conversation :replace T)))))

(defgeneric start-conversation (user participants title message)
  (:documentation "Start a new conversation with the given participants."))

(defmethod start-conversation ((user user) participants title message)
  (let ((*lquery-master-document*))
    (checked-request "/conversations/insert"
                     `(("recipients" . ,(to-participants participants))
                       ("title" . ,title)
                       ("message_html" . ,message)))
    (make-instance 'conversation
                   :id (let ((id ($ ".pageNavLinkGroup .linkGroup a" (first) (attr :href) (node))))
                         (subseq id (1+ (search "/" id)) (search "/" id :from-end T)))
                   :op ($ ".message" (first) (attr :data-author) (node))
                   :time ($ ".message" (first) ".messageMeta .datePermalink" (attr :data-time) (node))
                   :title ($ ".titleBar h1" (text) (node)))))

(defmethod get-posts ((conversation conversation) &key (start 0) (num 20))
  "Retrieve all posts in the given conversation."
  (let ((*lquery-master-document*))
    (checked-request (format NIL "/conversations/~a/" (id conversation)) NIL)
    (crawl-nodes ".messageList>li" #'(lambda (node) (make-meta-post node conversation 'conversation-post))
                 :start start :num num)))

(defmethod post ((conversation conversation) message &key)
  "Reply to a given conversation."
  (let ((*lquery-master-document*))
    (checked-request (concatenate 'string "/conversations/" (id conversation) "/insert-reply")
                     `(("message_html" . ,message)))
    (let ((post ($ ".messageList>li" (last))))
      (make-meta-post post conversation 'conversation-post))))

(defmethod reply ((post conversation-post) message &key)
  "Reply to a message in a conversation."
  (let ((message (format NIL "[quote=\"~a, convMessage: ~a\"]~a[/quote]~%~a"
                         (author post) (id post) (message post) message)))
    (post (thread post) message)))

(defgeneric invite (conversation participants)
  (:documentation "Invite a new user to the given conversation."))

(defmethod invite ((conversation conversation) participants)
  (let ((*lquery-master-document*))
    (assert (string-equal (id *user*) (op conversation)) ()
            'forum-error :code 4 :text (format NIL "Cannot invite users as you are not the OP (~a != ~a)." (id *user*) (op conversation)))
    (checked-request (concatenate 'string "/conversations/" (id conversation) "/invite-insert")
                     `(("recipients" . ,(to-participants participants))))
    NIL))

(defgeneric leave (conversation)
  (:documentation "Leave a given conversation."))

(defmethod leave ((conversation conversation))
  )
