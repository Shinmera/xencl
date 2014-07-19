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
           :start-thread
           :get-posts
           :get-threads
           :get-forums
           :get-user
           :post
           :reply
           :invite
           :shoutbox
           :forum
           :forum-thread
           :forum-post
           :*index*
           :*user*
           :user
           :forum-error :code :info :page
           :initiate
           :login
           :logout
           :id
           :pass
           :title
           :author
           :op
           :post-time
           :message))
