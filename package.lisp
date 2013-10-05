#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.ed-bot.xencl
  (:use :cl :lquery)
  (:nicknames :xencl :forum)
  (:export :conversation
           :conversations
           :start-conversation
           :get-posts
           :post
           :invite
           :reply
           :shoutbox
           :shoutbox-post
           :*index*
           :*user*
           :forum-error
           :initiate
           :login
           :logout
           :id
           :pass
           :author
           :op
           :post-time
           :message))
