#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
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

(defmethod conversations ((user user))
  "Retrieve the conversations the user has participated in. Note that this only works for the logged in user."
  (flet ((make-conversation (node)
           (make-instance 'conversation
                          :id (let ((id ($ node "h3.title a" (first) (attr :href) (node))))
                                (subseq id (1+ (search "/" id)) (search "/" id :from-end T)))
                          :op ($ node (attr :data-author) (node))
                          :time (let ((datetime ($ node ".posterDate .DateTime" (attr :data-time) (node))))
                                  (if (> (length datetime) 0)
                                      (local-time:unix-to-timestamp (parse-integer datetime))))
                          :title ($ node "h3.title a" (first) (text) (node)))))
    ($ (initialize (token-request "/conversations" NIL) :type :HTML))
    ($ ".discussionListItems li"
       (each #'make-conversation :replace T))))

(defmethod start-conversation ((user user) participants title message)
  "Start a new conversation with the given participants."
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

(defmethod get-posts ((conversation conversation) &key (start 0) (num -1))
  "Retrieve all posts in the given conversation."
  (let ((page (format NIL "/conversations/~a/" (id conversation))))
    ($ (initialize (token-request page NIL) :type :HTML))
    (when (search "Error" ($ "h1" (text) (node)))
      (error 'forum-error :code 404 :page page :info ($ "label.OverlayCloser" (text) (node))))
    (crawl-nodes ".messageList>li" #'(lambda (node) (make-meta-post node conversation 'conversation-post))
                 :start start :num num)))

(defmethod post ((conversation conversation) message &key)
  "Reply to a given conversation."
  (token-request (concatenate 'string "/conversations/" (id conversation) "/insert-reply")
                 `(("message_html" . ,message)))
  ;GET POST INSTANCE
  )

(defmethod invite ((conversation conversation) participants)
  "Invite a new user to the given conversation."
  (assert (string-equal (id *user*) (op conversation)) ()
          'forum-error :code 4 :text (format NIL "Cannot invite users as you are not the OP (~a != ~a)." (id *user*) (op conversation)))
  (token-request (concatenate 'string "/conversations/" (id conversation) "/invite-insert")
                 `(("recipients" . ,(to-participants participants))))
  NIL)
