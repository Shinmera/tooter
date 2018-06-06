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
  (let ((entity (allocate-instance type)))
    (decode-entity entity data)
    (initialize-instance entity)))

(defmethod decode-entity ((type symbol) (data list))
  (loop for entry in data
        collect (decode-entity type entry)))

(defmacro define-entity (name &body slots)
  (let ((data (gensym "DATA"))
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
                            (unless ,value
                              (setf (slot-value ,name ',slot)
                                    (funcall ,(or (getf options :translate-with)
                                                  '#'identity)
                                             ,value)))))
         ,name)

       (defun ,(intern (format NIL "~a-~a" 'decode name)) (,data)
         (decode-entity ',name ,data)))))

(define-entity account
  (id)
  (username)
  (account :field "acct")
  (display-name)
  (locked)
  (created-at :translate-with #'unix->universal)
  (followers-count)
  (following-count)
  (statuses-count)
  (note)
  (url)
  (avatar)
  (avatar-static)
  (header)
  (header-static)
  (moved :nullable T)
  (fields :nullable T)
  (bot :nullable T))

(define-entity application
  (name)
  (website :nullable T))

(define-entity attachment
  (id)
  (kind :field "type" :translate-with #'to-keyword)
  (url)
  (remote-url :nullable T)
  (preview-url)
  (text-url :nullable T)
  (metadata :field NIL :nullable T :translate-with #'decode-metadata)
  (description :nullable T))

(defvar *translator*)
(defun decode-metadata (data)
  (ecase (to-keyword (getj data "type"))
    (:image
     (let ((*translator* (lambda (data) (decode-entity 'image-metadata data))))
       (decode-entity 'metadata data)))
    ((:video :gifv)
     (let ((*translator* (lambda (data) (decode-entity 'video-metadata data))))
       (decode-entity 'metadata data)))
    (:unknown NIL)))

(define-entity metadata
  (small :nullable T :translate-with *translator*)
  (original :nullable T :translate-with *translator*)
  (focus :nullable T))

(define-entity image-metadata
  (width :nullable T)
  (height :nullable T)
  (size :nullable T)
  (aspect :nullable T))

(define-entity video-metadata
  (width :nullable T)
  (height :nullable T)
  (frame-rate :nullable T)
  (duration :nullable T)
  (bitrate :nullable T))

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

(define-entity context
  (ancestors :translate-with #'decode-status)
  (descendants :translate-with #'decode-status))

(define-entity emoji
  (shortcode)
  (static-url)
  (url))

(define-entity instance
  (uri)
  (title)
  (description)
  (email)
  (version)
  (urls)
  (languages)
  (contact-account :translate-with #'decode-account))

(define-entity user-list
  (id)
  (title))

(define-entity mention
  (url)
  (username)
  (account :field "acct")
  (id))

(define-entity notification
  (id)
  (kind :field "type" :translate-with #'to-keyword)
  (created-at :translate-with #'unix->universal)
  (account :translate-with #'decode-account)
  (status :translate-with #'decode-status :nullable T))

(define-entity push-subscription
  (id)
  (endpoint)
  (server-key)
  (alerts :nullable T))

(define-entity relationship
  (id)
  (following)
  (followed-by)
  (blocking)
  (muting)
  (muting-notifications)
  (requested)
  (domain-blocking))

(define-entity report
  (id)
  (action-taken))

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
  (reblog :nullable T)
  (content)
  (created-at :translate-with #'unix->universal)
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
  (language :nullable T)
  (pinned :nullable T))

(define-entity tag
  (name)
  (url)
  (history :translate-with #'tag-history :nullable T))

(define-entity tag-history
  (day :translate-with #'unix->universal)
  (uses)
  (accounts))
