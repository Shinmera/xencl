#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.ed-bot.xencl.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.ed-bot.xencl.asdf)

(defsystem xencl
  :name "XenForo Communication Library"
  :version "0.1.2"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Library for interaction with XenForo forums through CommonLisp."
  :long-description ""
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "meta")
               (:file "user")
               (:file "conversation")
               (:file "forum")
               (:file "shoutbox"))
  :depends-on (:drakma
               :cl-json
               :split-sequence
               :local-time
               :lquery
               :cl-ppcre))
