#|
 This file is a part of Tooter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.tooter)

(defclass entity ()
  ())

(defgeneric decode-entity (type data))

(defmethod decode-entity ((type symbol) data)
  (decode-entity (allocate-instance (find-class type)) data))

(defmethod decode-entity ((type symbol) (data list))
  (loop for entry in data
        collect (decode-entity type entry)))

(defmacro define-entity (name &body slots)
  (let ((*print-case* (readtable-case *readtable*))
        (data (gensym "DATA"))
        (value (gensym "VALUE")))
    `(progn
       (defclass ,name (entity)
         ,(loop for (slot . options) in slots
                collect `(,slot :initarg ,(intern (string slot) "KEYWORD")
                                :initform ,(if (getf options :nullable)
                                               NIL
                                               `(error ,(format NIL "~a required for a ~a." slot name)))
                                :accessor ,slot)))
       (defmethod decode-entity ((,name ,name) ,data)
         ,@(loop for (slot . options) in slots
                 for field = (getf options :field #1='#.(make-symbol "no value"))
                 collect `(let ((,value ,(cond ((null field) data)
                                               ((eq field #1#) `(getj ,data ,(translate-key slot)))
                                               (T `(getj ,data ,field)))))
                            (setf (slot-value ,name ',slot)
                                  (when ,value
                                    (funcall ,(or (getf options :translate-with)
                                                  '#'identity)
                                             ,value)))))
         ,name)

       (defun ,(intern (format NIL "~a-~a" 'decode name)) (,data)
         (decode-entity ',name ,data)))))

(define-entity field
  (name)
  (value)
  (verified-at :field "verified_at" :translate-with #'convert-timestamp :nullable T))

(defmethod print-object ((field field) stream)
  (print-unreadable-object (field stream :type T)
    (format stream "~a -> ~a verified-at ~a" (name field) (value field) (verified-at field))))

(define-entity account
  (id)
  (username)
  (account-name :field "acct")
  (display-name)
  (locked)
  (emojis :translate-with #'decode-emoji)
  (discoverable)
  (created-at :translate-with #'convert-timestamp)
  (followers-count)
  (following-count)
  (statuses-count)
  (note)
  (url)
  (avatar)
  (avatar-static)
  (header)
  (header-static)
  (moved  :nullable T :translate-with #'decode-account)
  (fields :nullable T :translate-with #'decode-field)
  (bot :nullable T)
  (source :nullable T))

(defmethod print-object ((account account) stream)
  (print-unreadable-object (account stream :type T)
    (format stream "~a #~a" (account-name account) (id account))))

(define-entity activity
  (week :translate-with #'convert-timestamp)
  (statuses)
  (logins)
  (registrations))

(defmethod print-object ((activity activity) stream)
  (with-accessors ((week week)
                   (statuses statuses)
                   (logins logins)
                   (registrations registrations)) activity
    (print-unreadable-object (activity stream :type T)
      (format stream "week ~a statuses ~a logins ~a registrations ~a"
              week statuses logins registrations))))

(define-entity application
  (name)
  (website :nullable T))

(defmethod print-object ((application application) stream)
  (print-unreadable-object (application stream :type T)
    (format stream "~a" (name application))))

(define-entity attachment
  (id)
  (kind :field "type" :translate-with #'to-keyword)
  (url)
  (remote-url :nullable T)
  (preview-url)
  (text-url :nullable T)
  (metadata :field NIL :nullable T :translate-with #'%decode-metadata)
  (description :nullable T))

(defmethod print-object ((attachment attachment) stream)
  (print-unreadable-object (attachment stream :type T)
    (format stream "~a #~a" (kind attachment) (id attachment))))

(defvar *translator*)
(defun %decode-metadata (data)
  (let ((metadata (getj data "meta")))
    (when metadata
      (let ((focus (getj metadata "focus")))
        (when focus
          (setf (gethash "focus" metadata) (cons (getj focus "x") (getj focus "y"))))
        (ecase (to-keyword (getj data "type"))
          (:image
           (let ((*translator* (lambda (data) (decode-entity 'image-metadata data))))
             (decode-entity 'metadata metadata)))
          ((:video :gifv)
           (let ((*translator* (lambda (data) (decode-entity 'video-metadata data))))
             (decode-entity 'metadata metadata)))
          (:unknown NIL))))))

(define-entity metadata
  (small :nullable T :translate-with *translator*)
  (original :nullable T :translate-with *translator*)
  (focus :nullable T))

(define-entity image-metadata
  (width :nullable T)
  (height :nullable T)
  (size :nullable T)
  (aspect :nullable T))

(defmethod print-object ((image-metadata image-metadata) stream)
  (print-unreadable-object (image-metadata stream :type T)
    (format stream "~ax~a" (width image-metadata) (height image-metadata))))

(define-entity video-metadata
  (width :nullable T)
  (height :nullable T)
  (frame-rate :nullable T)
  (duration :nullable T)
  (bitrate :nullable T))

(defmethod print-object ((video-metadata video-metadata) stream)
  (print-unreadable-object (video-metadata stream :type T)
    (format stream "~ax~a~@[@~afps~]~@[ ~as~]" (width video-metadata) (height video-metadata)
            (frame-rate video-metadata) (duration video-metadata))))

(define-entity card
  (url)
  (title)
  (description)
  (image :nullable T)
  (kind :field "type" :translate-with #'to-keyword)
  (author-name :nullable T)
  (author-url :nullable T)
  (provider-name :nullable T)
  (provider-url :nullable T)
  (html :nullable T)
  (width :nullable T)
  (height :nullable T))

(defmethod print-object ((card card) stream)
  (print-unreadable-object (card stream :type T)
    (format stream "~a ~a" (kind card) (title card))))

(define-entity context
  (ancestors :translate-with #'decode-status)
  (descendants :translate-with #'decode-status))

(define-entity emoji
  (shortcode)
  (static-url)
  (url))

(defmethod print-object ((emoji emoji) stream)
  (print-unreadable-object (emoji stream :type T)
    (format stream "~a" (shortcode emoji))))

(defun translate-languages (languages)
  (mapcar #'to-keyword languages))

(define-entity instance
  (uri)
  (title)
  (description)
  (email)
  (version)
  (urls)
  (languages :translate-with #'translate-languages)
  (contact-account :translate-with #'decode-account))

(defmethod print-object ((instance instance) stream)
  (print-unreadable-object (instance stream :type T)
    (format stream "~s ~a" (title instance) (uri instance))))

(define-entity user-list
  (id)
  (title))

(defmethod print-object ((user-list user-list) stream)
  (print-unreadable-object (user-list stream :type T)
    (format stream "~s #~a" (title user-list) (id user-list))))

(define-entity mention
  (url)
  (username)
  (account-name :field "acct")
  (id))

(defmethod print-object ((mention mention) stream)
  (print-unreadable-object (mention stream :type T)
    (format stream "~a #~a" (account-name mention) (id mention))))

(define-entity notification
  (id)
  (kind :field "type" :translate-with #'to-keyword)
  (created-at :translate-with #'convert-timestamp)
  (account :translate-with #'decode-account)
  (status :translate-with #'decode-status :nullable T))

(defmethod print-object ((notification notification) stream)
  (print-unreadable-object (notification stream :type T)
    (format stream "~a ~a #~a" (kind notification) (account-name (account notification))
            (id notification))))

(define-entity push-subscription
  (id)
  (endpoint)
  (server-key)
  (alerts :nullable T))

(defmethod print-object ((push-subscription push-subscription) stream)
  (print-unreadable-object (push-subscription stream :type T)
    (format stream "~a #~a" (endpoint push-subscription) (id push-subscription))))

(define-entity relationship
  (id)
  (following)
  (followed-by :field "followed_by")
  (blocking)
  (muting)
  (muting-notifications)
  (requested)
  (domain-blocking))

(defmethod print-object ((relationship relationship) stream)
  (print-unreadable-object (relationship stream :type T)
    (format stream "#~a" (id relationship))))

(define-entity report
  (id)
  (action-taken))

(defmethod print-object ((report report) stream)
  (print-unreadable-object (report stream :type T)
    (format stream "~a #~a" (action-taken report) (id report))))

(define-entity results
  (accounts :translate-with #'decode-account)
  (statuses :translate-with #'decode-status)
  (hashtags))

(define-entity status
  (id)
  (uri)
  (url :nullable T)
  (account :translate-with #'decode-account)
  (in-reply-to-id :nullable T)
  (in-reply-to-account-id :nullable T)
  (parent :field "reblog" :nullable T :translate-with #'decode-status)
  (content)
  (created-at :translate-with #'convert-timestamp)
  (emojis :translate-with #'decode-emoji)
  (reblogs-count)
  (favourites-count)
  (reblogged :nullable T)
  (favourited :nullable T)
  (muted :nullable T)
  (sensitive)
  (spoiler-text)
  (visibility :translate-with #'to-keyword)
  (media-attachments :translate-with #'decode-attachment)
  (mentions :translate-with #'decode-mention)
  (tags :translate-with #'decode-tag)
  (application :translate-with #'decode-application :nullable T)
  (language :nullable T :translate-with #'to-keyword)
  (pinned :nullable T))

(defmethod print-object ((status status) stream)
  (print-unreadable-object (status stream :type T)
    (format stream "~a #~a" (account-name (account status)) (id status))))

(defmethod describe-object ((status status) stream)
  (format stream "~s~%~%" status)
  (present status stream))

(defmethod present ((status status) (stream stream))
  (let ((account (account status))
        (width (or *print-right-margin* 80)))
    (multiple-value-bind (ss mm hh d m y) (decode-universal-time (created-at status) 0)
      (format stream
              "~v<~a @~a ~;~d.~d.~d ~d:~2,'0d:~2,'0d~>
~a
~v<~d♺ ~d❤~>"
              width
              (display-name account)
              (account-name account)
              y m d hh mm ss
              (line-wrap (plain-format-html (content status))
                         :prefix "| " :width width)
              width
              (reblogs-count status)
              (favourites-count status)))))

(define-entity tag
  (name)
  (url)
  (history :translate-with #'decode-tag-history :nullable T))

(defmethod print-object ((tag tag) stream)
  (print-unreadable-object (tag stream :type T)
    (format stream "~a" (name tag))))

(define-entity tag-history
  (day :translate-with #'convert-timestamp)
  (use-count :field "uses")
  (account-count :field "accounts"))

(defmethod print-object ((tag-history tag-history) stream)
  (print-unreadable-object (tag-history stream :type T)
    (with-accessors ((day day)
                     (use-count use-count)
                     (account-count account-count)) tag-history
      (format stream
              "day: ~a use-count: ~a account-count: ~a"
              day use-count account-count))))

(define-entity conversation
  (id)
  (accounts :translate-with #'decode-account)
  (unread)
  (last-status :field "last_status" :translate-with #'decode-status))

(defmethod print-object ((conversation conversation) stream)
  (print-unreadable-object (conversation stream :type T)
    (with-accessors ((id id)
                     (accounts accounts)
                     (last-status last-status)
                     (unread unread)) conversation
      (format stream "~a unread? ~a ~a ~a" id unread accounts last-status))))
