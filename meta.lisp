#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.xencl)

(defclass meta-thread ()
  ()
  (:documentation "Super-class for thread-like classes."))

(defgeneric get-posts (object &key &allow-other-keys))
(defgeneric post (object message &key &allow-other-keys))

(defclass meta-post ()
  ((id :initarg :id :accessor id)
   (author :initarg :author :reader author)
   (message :initarg :message :accessor message)
   (time :initarg :time :accessor post-time))
  (:documentation "Super-class for post-like classes."))

(defgeneric reply (object message &key &allow-other-keys))
