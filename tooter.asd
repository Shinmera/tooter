#|
 This file is a part of Tooter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem tooter
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A client library for Mastodon instances."
  :homepage "https://github.com/Shinmera/tooter"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "client")
               (:file "objects")
               (:file "queries")
               (:file "documentation"))
  :depends-on (:yason
               :cl-ppcre
               :drakma
               :documentation-utils))
