#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage :org.tymoonnext.ed-bot.xencl.doc
  (:use :cl :lquery :lquery-doc)
  (:nicknames :xencl-doc)
  (:export :build-documentation))

(in-package :org.tymoonnext.ed-bot.xencl.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object)))))

(defun build-documentation ()
  (write-documentation :xencl
                       (merge-pathnames "about-template.html" (asdf:system-source-directory :xencl))
                       :output-file (merge-pathnames "about.html" (asdf:system-source-directory :xencl))
                       :exclude '(:internal :method)))
