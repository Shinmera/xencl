#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.ed-bot.xencl.doc.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.ed-bot.xencl.doc.asdf)

(defsystem xencl-doc
  :name "XenCL Doc"
  :components ((:file "documentation"))
  :depends-on (:text-compare :lquery :lquery-doc))
