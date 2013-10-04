#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defclass shoutbox (meta-thread)
  ()
  (:documentation "Shoutbox class for interacting with the XenForo taigachat."))

(defclass shoutbox-post (meta-post)
  ()
  (:documentation "Shoutbox post class."))

(defmethod print-object ((shoutbox-post shoutbox-post) out)
  (print-unreadable-object (shoutbox-post out :type T)
    (format out "#~a ~a <~a>" (id shoutbox-post) (local-time:format-timestring NIL (post-time shoutbox-post) :format '(:hour #\: :min)) (author shoutbox-post))))

(defmethod get-posts ((shoutbox shoutbox) &key (last-post 0))
  (labels ((parse-timestring (timestring)
             (let* ((timestring (subseq timestring 0 (search " - " timestring)))
                    (time-ampm (split-sequence:split-sequence #\Space timestring))
                    (time (split-sequence:split-sequence #\: (first time-ampm)))
                    (am (string= "AM" (second time-ampm)))
                    (h (parse-integer (first time)))
                    (h (if am h (+ h 12)))
                    (h (if (>= h 24) 0 h))
                    (m (parse-integer (second time)))
                    (now (local-time:now)))
               (format T "~a ~a ~%" h m)
               (local-time:adjust-timestamp! now (set :hour h) (set :minute m) (set :sec 0))))

           (make-post (li)
             (make-instance 'shoutbox-post
                            :id (parse-integer (subseq ($ li (attr :id) (node)) (length "taigachat_message_")))
                            :author ($ li ".username" (text) (node))
                            :time (parse-timestring ($ li ".DateTime" (text) (node)))
                            :message ($ li ".taigachat_messagetext" (text) (node))))

           (make-posts (html)
             ($ (initialize (concatenate 'string "<ul>" html "</ul>")))
             (remove-if #'(lambda (post) (<= (id post) last-post))
              ($ "li" (each #'make-post :replace T)))))

    (let ((json
           (cl-json:decode-json
            (token-request "/index.php?taigachat/list.json"
                           `(("_xfRequestUri" . ,(url "/shoutbox"))
                             ("_xfNoRedirect" . "1")
                             ("sidebar" . "0")
                             ("lastrefresh" . ,(format NIL "~s" last-post))) :stream T))))
      (values (make-posts (cdr (assoc :template-html json)))
              (cdr (assoc :lastrefresh json))))))

(defun shoutbox-post (message)
  (cl-json:decode-json
   (token-request "/index.php?taigachat/post.json"
                  `(("_xfRequestUri" . ,(url "/shoutbox"))
                    ("_xfNoRedirect" . "1")
                    ("sidebar" . "0")
                    ("message" . ,message)) :stream T)))

(defmethod post ((shoutbox shoutbox) message &key)
  (shoutbox-post message))

(defmethod reply ((shoutbox-post shoutbox-post) message &key)
  (shoutbox-post (format NIL "~a: ~a" (author shoutbox-post) message)))
