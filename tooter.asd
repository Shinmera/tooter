(asdf:defsystem tooter
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A client library for Mastodon instances."
  :homepage "https://shinmera.com/project/tooter"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "link-header-parser")
               (:file "client")
               (:file "objects")
               (:file "queries")
               (:file "v2")
               (:file "v6")
               (:file "documentation"))
  :depends-on (:alexandria
               :yason
               :cl-ppcre
               :dexador
               :documentation-utils))
