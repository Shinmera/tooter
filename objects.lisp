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
  (source :nullable T :translate-with #'decode-source))

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

(define-entity announcement-account
  (id)
  (username)
  (account-name :field "acct")
  (url))

(defmethod print-object ((announcement-account announcement-account) stream)
  (with-accessors ((id id)
                   (username username)
                   (account-name account-name)
                   (url url)) announcement-account
    (print-unreadable-object (announcement-account stream :type T)
      (format stream
              "#~a username: ~a account-name: ~a url: ~a"
              id username account-name url))))

(define-entity announcement-status
  (id)
  (url))

(defmethod print-object ((announcement-status announcement-status) stream)
  (with-accessors ((id id)
                   (url url)) announcement-status
    (print-unreadable-object (announcement-status stream :type T)
      (format stream "#~a url: ~a" id url))))

(define-entity announcement
  (id)
  (content)
  (starts-at :translate-with #'convert-timestamp :nullable t)
  (ends-at :translate-with #'convert-timestamp :nullable t)
  (published)
  (all-day)
  (published-at :translate-with #'convert-timestamp)
  (updated-at :translate-with #'convert-timestamp)
  (readp :field "read")
  (mentions :translate-with #'decode-announcement-account)
  (statuses :translate-with #'decode-announcement-status)
  (tags :translate-with #'decode-tag)
  (emojis :translate-with #'decode-emoji)
  (reactions :translate-with #'decode-reaction))

(defmethod print-object ((announcement announcement) stream)
  (with-accessors ((id id)
                   (content content)) announcement
    (print-unreadable-object (announcement stream :type T)
      (format stream "#~a already read? ~a content: ~a" id (readp announcement) content))))

(define-entity application
  (name)
  (website :nullable T)
  (scopes)
  (redirect-uris :field "redirect_uris"))

(defmethod print-object ((application application) stream)
  (print-unreadable-object (application stream :type T)
    (format stream "~a ~@[website: ~a~]" (name application) (website application))))

(define-entity attachment
  (id)
  (kind :field "type" :translate-with #'to-keyword)
  (url)
  (preview-url)
  (remote-url :nullable T)
  (text-url :nullable T)
  (metadata :field NIL :nullable T :translate-with #'%decode-metadata)
  (description :nullable T)
  (blurhash :nullable T))

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
          (:audio
           (let ((*translator* (lambda (data) (decode-entity 'audio-metadata data))))
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

(define-entity audio-metadata
  (audio-length :field "length" :nullable T)
  (duration :nullable T)
  (audio-encode :field "audio_encode" :nullable T)
  (audio-bitrate :field "audio_bitrate" :nullable T)
  (audio-channels :field "audio_channels" :nullable T))

(defmethod print-object ((audio-metadata audio-metadata) stream)
  (print-unreadable-object (audio-metadata stream :type T)
    (format stream "length ~a encode ~a" (audio-length audio-metadata) (audio-encode audio-metadata))))

(define-entity card
  (url)
  (title)
  (description)
  (kind :field "type" :translate-with #'to-keyword)
  (author-name :nullable T)
  (author-url :nullable T)
  (provider-name :nullable T)
  (provider-url :nullable T)
  (html :nullable T)
  (width :nullable T)
  (height :nullable T)
  (image :nullable T)
  (embed-url :field "embed_url" :nullable T))

(defmethod print-object ((card card) stream)
  (print-unreadable-object (card stream :type T)
    (format stream "~a ~a" (kind card) (title card))))

(define-entity context
  (ancestors :translate-with #'decode-status)
  (descendants :translate-with #'decode-status))

(define-entity emoji
  (shortcode)
  (url)
  (static-url)
  (visible-in-picker :field "visible_in_picker")
  (category :nullable T))

(defmethod print-object ((emoji emoji) stream)
  (print-unreadable-object (emoji stream :type T)
    (format stream "~a" (shortcode emoji))))

(defun translate-languages (languages)
  (mapcar #'to-keyword languages))

;; according to  documentation the fields  of this entity  are numbers
;; (and integer, given the description)
(define-entity instance-stats
  (user-count :field "user_count")
  (status-count :field "status_count")
  (domain-count :field "domain_count"))

(defmethod print-object ((instance-stats instance-stats) stream)
  (print-unreadable-object (instance-stats stream :type T)
    (format stream "user count ~a status count ~a domain count ~a"
            (user-count instance-stats)
            (status-count instance-stats)
            (domain-count instance-stats))))

(defun %decode-configuration (data)
  (let ((accounts (gethash "accounts" data))
        (statuses (gethash "statuses" data))
        (media-attachments (gethash "media_attachments" data))
        (polls (gethash "polls" data))
        (translation (gethash "translation" data))
        (urls (gethash "urls" data))
        (vapid (gethash "vapid" data)))
    (append
     (when accounts
       (list :accounts (list :max-featured-tags (gethash "max_featured_tags" accounts)
                             :max-pinned-status (gethash "max_pinned-status" accounts))))
     (when statuses
       (list :statuses (list :max-characters    (gethash "max_characters" statuses)
                             :max-media-attachments (gethash "max_media_attachments" statuses)
                             :characters-reserved-per-url
                             (gethash "characters_reserved_per_url" statuses))))
     (when media-attachments
       (list :media-attachments (list :supported-mime-types
                                      (gethash "supported_mime_types" media-attachments)
                                      :image-size-limit
                                      (gethash "image_size_limit" media-attachments)
                                      :image-matrix-limit
                                      (gethash "image_matrix_limit" media-attachments)
                                      :video-size-limit
                                      (gethash "video_size_limit" media-attachments)
                                      :video-frame-rate-limit
                                      (gethash "video_frame_rate_limit" media-attachments)
                                      :video-matrix-limit
                                      (gethash "video_matrix_limit" media-attachments))))
     (when polls
       (list :polls (list :max-options (gethash "max-options" polls)
                          :max-characters-per-option
                          (gethash "max_characters_per_option" polls)
                          :min-expiration (gethash "min_expiration" polls)
                          :max-expiration (gethash "max_expiration" polls))))
     (when translation
       (list :translation (list :enabled (gethash "enabled" translation))))
     (when urls
       (list :urls (list :streaming (gethash "streaming" urls)
                         :status (gethash "status" urls)))) ; undocumented, 2024-10-12
     (when vapid
       (list :vapid (list :public-key (gethash "public_key" vapid)))))))

(defun %decode-registrations (data)
  (let ((registrations data))
    (when (hash-table-p registrations)
      (list :enabled (gethash "enabled" registrations)
            :approval-required (gethash "approval_required" registrations)
            :message (gethash "message" registrations)))))

(defun %decode-api-versions (data)
  (let ((api-versions data))
    (when (hash-table-p api-versions)
      (list :api-versions (list :mastodon (gethash "mastodon" api-versions))))))

(defun %decode-contact (data)
  (let ((contact data))
    (when (hash-table-p contact)
      (list :email (gethash "email" contact)
            :account  (and (gethash "account" contact)
                           (decode-account (gethash "account" contact)))))))

(define-entity instance-rule
  (id)
  (text)
  (hint))

(defmethod print-object ((instance-rule instance-rule) stream)
  (with-accessors ((id id)
                   (text text)
                   (hint hint)) instance-rule
    (print-unreadable-object (instance-rule stream :type T)
      (format stream "rule #~a text ~s hint ~s" id text hint))))

(defun %decode-usage (data)
  (when (hash-table-p data)
    (let ((users (gethash "users" data)))
      (when (hash-table-p users)
        (list :active-month (gethash "active_month" users))))))

(defun %decode-thumbnail (data)
  (when (hash-table-p data)
    (let* ((thumbnail data)
           (versions (gethash "versions" thumbnail)))
      (list :thumbnail
            (append (list :url (gethash "url" thumbnail)
                          :blurhash (gethash "blurhash" thumbnail))
                    (when (hash-table-p versions)
                      (list :@1x (gethash "@1x" versions)
                            :@2x (gethash "@2x" versions))))))))

(defun %decode-instance-icon-size (data)
  (let ((multiplication-symbol-pos (position #\x data :test #'char-equal)))
    (if multiplication-symbol-pos
        (list (subseq data 0 multiplication-symbol-pos)
              (subseq data (1+ multiplication-symbol-pos)))
        data)))

(define-entity instance-icon
  (icon-src :field "src")
  (icon-size :field "size" :translate-with #'%decode-instance-icon-size))

(defmethod print-object ((instance-icon instance-icon) stream)
  (print-unreadable-object (instance-icon stream :type T)
    (format t "url ~a size ~a" (icon-src instance-icon) (icon-size instance-icon))))

(define-entity instance
  (domain)
  (title)
  (version)
  (source-url :field "source_url")
  (description)
  (usage :translate-with #'%decode-usage)
  (thumbnail :nullable T :translate-with #'%decode-thumbnail)
  (icon :translate-with #'decode-instance-icon)
  (languages :translate-with #'translate-languages)
  (configuration :translate-with #'%decode-configuration)
  (registrations :translate-with #'%decode-registrations)
  (api-versions :field "api_versions" :translate-with #'%decode-api-versions)
  (contact :translate-with #'%decode-contact)
  (rules :translate-with #'decode-instance-rule))

(defmethod print-object ((instance instance) stream)
  (print-unreadable-object (instance stream :type T)
    (format stream
            "~s ~a configuration ~a"
            (title instance)
            (domain instance)
            (configuration instance))))

(define-entity user-list
  (id)
  (title))

(defmethod print-object ((user-list user-list) stream)
  (print-unreadable-object (user-list stream :type T)
    (format stream "~s #~a" (title user-list) (id user-list))))

(define-entity marker
  (last-read-id :field "last_read_id")
  (updated-at :field "updated_at" :translate-with #'convert-timestamp)
  (version))

(defmethod print-object ((marker marker) stream)
  (print-unreadable-object (marker stream :type T)
    (format stream "last read id ~a  uptdated at ~a"
            (last-read-id marker) (updated-at marker))))

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
  (group-id :field "group_id")
  (created-at :translate-with #'convert-timestamp)
  (account :translate-with #'decode-account)
  (status :translate-with #'decode-status :nullable T)
  (report :translate-with #'decode-report :nullable T)
  (relationship-severance-event :field "relationship_severance_event"
                                :translate-with #'decode-relationship-severance-event
                                :nullable t)
  (moderation-warning :field "moderation_warning"
                      :translate-with #'decode-account-warning
                      :nullable t))

(defmethod print-object ((notification notification) stream)
  (print-unreadable-object (notification stream :type T)
    (format stream
            "~a ~a #~a ~@[report: ~a~] ~@[moderation warning: ~a~]"
            (kind notification)
            (account-name (account notification))
            (id notification)
            (report notification)
            (moderation-warning notification))))

(define-entity poll-option
  (title)
  (votes-count :field "votes_count"))

(defmethod print-object ((poll-option poll-option) stream)
  (print-unreadable-object (poll-option stream :type T)
    (format stream "title ~a count ~a" (title poll-option) (votes-count poll-option))))

(define-entity poll
  (id)
  (expires-at :field "expires_at" :translate-with #'convert-timestamp)
  (expired)
  (multiple)
  (voters-count :field "voters_count")
  (votes-count :field "votes_count")
  (voted)
  (own-votes :field "own_votes")
  (options :translate-with #'decode-poll-option)
  (emojis :translate-with #'decode-emoji))

(defmethod print-object ((poll poll) stream)
  (print-unreadable-object (poll stream :type T)
    (format stream
            "#~a voted? ~a voters count ~a votes count ~a"
            (id poll)
            (voted poll)
            (voters-count poll)
            (votes-count  poll))))

(define-entity preferences
  (posting-default-visibility :field "posting:default:visibility" :translate-with #'to-keyword)
  (posting-default-sensitive :field "posting:default:sensitive")
  (posting-default-language :field "posting:default:language" :translate-with #'to-keyword :nullable T)
  (reading-expand-media :field "reading:expand:media" :translate-with #'to-keyword)
  (reading-expand-spoilers :field "reading:expand:spoilers"))

(defmethod print-object ((preferences preferences) stream)
  (print-unreadable-object (preferences stream :type T)
    (with-accessors ((posting-default-visibility posting-default-visibility)
                     (posting-default-sensitive posting-default-sensitive)
                     (posting-default-language posting-default-language)
                     (reading-expand-media reading-expand-media)
                     (reading-expand-spoilers reading-expand-spoilers)) preferences
      (format stream "posting default visibility ~a posting default sensitive ~a posting default language ~a reading expand media ~a reading expand spoilers ~a"
              posting-default-visibility
              posting-default-sensitive
              posting-default-language
              reading-expand-media
              reading-expand-spoilers))))

(define-entity push-subscription-alerts
  (alert-follow)
  (alert-favourite)
  (alert-mention)
  (alert-reblog)
  (alert-poll))

(defmethod print-object ((push-subscription-alerts push-subscription-alerts) stream)
  (print-unreadable-object (push-subscription-alerts stream :type T)
    (with-accessors ((alert-follow alert-follow)
                     (alert-favourite alert-favourite)
                     (alert-mention alert-mention)
                     (alert-reblog alert-reblog)
                     (alert-poll alert-poll)) push-subscription-alerts
      (format stream "when follow? ~a when favourite ~a when mention? ~a when reblog? ~a when poll? ~a"
              alert-follow alert-favourite alert-mention alert-reblog alert-poll))))

(define-entity push-subscription
  (id)
  (endpoint)
  (server-key)
  (alerts :nullable T :translate-with #'decode-push-subscription-alerts))

(defmethod print-object ((push-subscription push-subscription) stream)
  (print-unreadable-object (push-subscription stream :type T)
    (format stream "~a #~a" (endpoint push-subscription) (id push-subscription))))

(define-entity reaction
  (name)
  (reaction-count :field "count")
  (me :nullable T)
  (url :nullable T)
  (static-url :nullable T))

(defmethod print-object ((reaction reaction) stream)
  (print-unreadable-object (reaction stream :type T)
    (format stream
            "me? ~a name: ~a count: ~a url: ~a static_url ~a"
            (me reaction)
            (name reaction)
            (reaction-count reaction)
            (url reaction)
            (static-url reaction))))

(define-entity relationship
  (id)
  (following)
  (requested)
  (endorsed)
  (followed-by :field "followed_by")
  (muting)
  (muting-notifications)
  (showing-reblogs :field "showing_reblogs")
  (blocking)
  (domain-blocking)
  (blocked-by :field "blocked_by"))

(defmethod print-object ((relationship relationship) stream)
  (print-unreadable-object (relationship stream :type T)
    (format stream "#~a" (id relationship))))

(define-entity report
  (id)
  (action-taken)
  (action-taken-at :field "action_taken_at" :translate-with #'convert-timestamp)
  (report-category :field "category" :translate-with #'to-keyword)
  (comment)
  (forwarded)
  (created-at :field "created_at" :translate-with #'convert-timestamp)
  (status-ids :field "status_ids" :nullable T)
  (rule-ids :field "rule_ids" :nullable T)
  (target-account :field "target_account" :translate-with #'decode-account))

(defmethod print-object ((report report) stream)
  (with-accessors ((id id)
                   (action-taken action-taken)
                   (category category)
                   (comment comment)) report
    (print-unreadable-object (report stream :type T)
      (format stream
              "#~a action taken? ~a category: ~a comment ~a"
              id
              action-taken
              category
              comment))))

(define-entity results
  (results-accounts :field "accounts" :translate-with #'decode-account)
  (results-statuses :field "statuses" :translate-with #'decode-status)
  (results-tags     :field "hashtags" :translate-with #'decode-tag))

(defmethod print-object ((results results) stream)
  (print-unreadable-object (results stream :type T)
    (format stream
            "account ~a statuses ~a hashtags ~a"
            (results-accounts results)
            (results-statuses results)
            (results-tags results))))

(define-entity status-params
  (text)
  (in-reply-to-id :field "in_rein_reply_to_id" :nullable T)
  (media-ids :field "media_ids" :nullable T)
  (sensitive :nullable T)
  (spoiler-text :field "spoiler_text" :nullable T)
  (visibility :translate-with #'to-keyword)
  (scheduled-at :field "scheduled_at" :nullable T :translate-with #'convert-timestamp)
  (application-id :field "application_id"))

;; TODO check official documentation because currently is WIP 2020-08-25
(define-entity scheduled-status
  (id)
  (scheduled-at :field "scheduled_at" :translate-with #'convert-timestamp)
  (params :translate-with #'decode-status-params))

(defmethod print-object ((scheduled-status scheduled-status) stream)
  (print-unreadable-object (scheduled-status stream :type T)
    (format stream "~a@~a" (id scheduled-status) (scheduled-at scheduled-status))))

(define-entity status-tag
  (name)
  (url))

(defmethod print-object ((status-tag status-tag) stream)
  (print-unreadable-object (status-tag stream :type T)
    (format stream "name ~a url ~a" (name status-tag) (url status-tag))))

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
  (replies-count :field "replies_count" :translate-with #'ensure-integer)
  (reblogged :nullable T)
  (favourited :nullable T)
  (muted :nullable T)
  (sensitive)
  (spoiler-text)
  (visibility :translate-with #'to-keyword)
  (media-attachments :translate-with #'decode-attachment)
  (mentions :translate-with #'decode-mention)
  (tags :translate-with #'decode-status-tag)
  (application :translate-with #'decode-application :nullable T)
  (language :nullable T :translate-with #'to-keyword)
  (pinned :nullable T)
  (poll :nullable T :translate-with #'decode-poll)
  (preview-card :field "card" :nullable T :translate-with #'decode-card)
  (bookmarked)
  (filtered :nullable T :translate-with #'decode-filter-results))

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

(define-entity source
  (note)
  (fields :translate-with #'decode-field)
  (privacy :translate-with #'to-keyword)
  (sensitive)
  (language :translate-with #'to-keyword)
  (follow-requests-count :fields "follow_requests_count" :translate-with #'ensure-integer))

(defmethod print-object ((source source) stream)
  (print-unreadable-object (source stream :type T)
    (format stream "~a" (note source))))

(define-entity tag
  (name)
  (url)
  (history :translate-with #'decode-tag-history :nullable T)
  (following))

(defmethod print-object ((tag tag) stream)
  (print-unreadable-object (tag stream :type T)
    (format stream "~a following? ~a" (name tag) (following tag))))

(define-entity tag-history
  (day :translate-with (lambda (a) (convert-timestamp (parse-integer a))))
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

(define-entity featured-tag
  (id)
  (name)
  (statuses-count :field "statuses_count" :translate-with #'ensure-integer)
  (last-status-at :field "last_status_at" :translate-with #'convert-timestamp))

(defmethod print-object ((featured-tag featured-tag) stream)
  (print-unreadable-object (featured-tag stream :type T)
    (with-accessors ((id id)
                     (name name)
                     (statuses-count statuses-count)
                     (last-status-at last-status-at)) featured-tag
      (format stream
              "~a name ~a  count ~a last status at ~a"
              id name statuses-count last-status-at))))

(define-entity filter
  (id)
  (title)
  (filter-context)
  (expires-at :field "expires_at" :translate-with #'convert-timestamp)
  (filter-action :field "filter_action" :translate-with #'to-keyword)
  (keywords :translate-with #'decode-filter-keyword)
  (statuses :translate-with #'decode-filter-status))

(defmethod print-object ((filter filter) stream)
  (print-unreadable-object (filter stream :type T)
    (with-accessors ((id id)
                     (title title)
                     (filter-context filter-context)
                     (expires-at expires-at)
                     (filter-action filter-action)
                     (keywords keywords)
                     (statuses statuses)) filter
      (format stream
              "~a title ~a context ~a expires at ~a actions ~a keywords ~a statuses ~a"
              id title filter-context expires-at filter-action keywords statuses))))

(define-entity filter-results
  (query-filter :translate-with #'decode-filter)
  (keyword-matches :field "keyword_matches" :nullable T)
  (status-matches :field "status_matches" :nullable T))

(defmethod print-object ((filter-results filter-results) stream)
  (print-unreadable-object (filter-results stream :type T)
    (with-accessors ((filter query-filter)
                     (keyword-matches keyword-matches)
                     (status-matches status-matches)) filter-results
      (format stream
              "filter: ~a matched keywords: ~a matched status: ~a"
              filter keyword-matches status-matches))))

(define-entity filter-keyword
  (id)
  (status-id)
  (whole-word :field "whole_word"))

(defmethod print-object ((filter-keyword filter-keyword) stream)
  (print-unreadable-object (filter-keyword stream :type T)
    (with-accessors ((id id)
                     (status-id status-id)
                     (whole-word whole-word)) filter-keyword
      (format stream "id: ~a status-id: ~a whole word? ~a" id status-id whole-word))))

(define-entity filter-status
  (id)
  (status-id))

(defmethod print-object ((filter-status filter-status) stream)
  (print-unreadable-object (filter-status stream :type T)
    (with-accessors ((id id)
                     (status-id status-id)) filter-status
      (format stream "id: ~a status-id: ~a" id status-id))))

(define-entity identity-proof
  (provider)
  (provider-username :field "provider_username")
  (profile-url :field "profile_url")
  (proof-url :field "proof_url")
  (updated-at :field "updated_at" :translate-with #'convert-timestamp))

(defmethod print-object ((identity-proof identity-proof) stream)
  (print-unreadable-object (identity-proof stream :type T)
    (with-accessors ((provider provider)
                     (provider-username provider-username)
                     (profile-url profile-url)
                     (proof-url proof-url)
                     (updated-at updated-at)
                     (irreversible irreversible)
                     (whole-word whole-word)) identity-proof
      (format stream
              "provider ~a provider username ~a profile url ~a proof url ~a  updated at ~a"
              provider provider-username profile-url proof-url updated-at))))

(define-entity token
  (access-token :field "access_token")
  (token-type :field "token_type")
  (scope)
  (created-at :field "created_at" :translate-with #'convert-timestamp))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type T)
    (with-accessors ((access-token access-token)
                     (token-type token-type)
                     (scope scope)
                     (created-at created-at)) token
      (format stream
              "access token ~a type ~a scope ~a created at ~a"
              access-token token-type scope created-at))))

(define-entity relationship-severance-event
  (id)
  (kind :field "type" :translate-with #'to-keyword)
  (purged)
  (target-name :field "target_name")
  (relationships-count :field "relationships_count" :nullable t)
  (created-at :field "created_at" :translate-with #'convert-timestamp))

(defmethod print-object ((relationship-severance-event relationship-severance-event) stream)
  (print-unreadable-object (relationship-severance-event stream :type T)
    (with-accessors ((id id)
                     (kind kind)
                     (relationships-count relationships-count)) relationship-severance-event
      (format stream
              "~a type ~a ~@[count ~a~]"
              id
              kind
              relationships-count))))

(define-entity account-warning
  (id)
  (action :translate-with #'to-keyword)
  (text)
  (status-ids :field "status_ids")
  (target-account :field "target_account" :translate-with #'decode-account)
  (appeal :nullable t :translate-with #'decode-appeal)
  (created-at :field "created_at" :translate-with #'convert-timestamp))

(defmethod print-object ((account-warning account-warning) stream)
  (print-unreadable-object (account-warning stream :type T)
    (with-accessors ((id id)
                     (action action)
                     (text text)) account-warning
      (format stream
              "~a action ~a text ~a"
              id
              action
              text))))

(define-entity appeal
  (text)
  (state))

(defmethod print-object ((account-warning account-warning) stream)
  (print-unreadable-object (account-warning stream :type T)
      (format stream
              "text: ~a state: ~a"
              (text account-warning)
              (state account-warning))))

(define-entity partial-account-with-avatar
  (id)
  (account-name :field "acct")
  (url)
  (avatar)
  (avatar-static)
  (locked)
  (display-name)
  (bot))

(define-entity notification-group
  (group-key :field "group_key")
  (notifications-count :field "notifications_count")
  (kind :field "type")
  (most-recent-notification-id :field "most_recent_notification_id")
  (page-min-id :field "page_min_id" :nullable t)
  (page-max-id :field "page_max_id" :nullable t)
  (latest-page-notification-at :field "latest_page_notification_at" :nullable t)
  (sample-account-ids :field "sample_account_ids")
  (status-id :field "status_id" :nullable t)
  (report :translate-with #'decode-report :nullable t)
  (relationship-severance-event :field "event"
                                :translate-with #'decode-relationship-severance-event
                                :nullable t)
  (moderation-warning :field "moderation_warning" :nullable t))

(define-entity grouped-notifications-results
  (accounts :translate-with #'decode-account)
  (partial-accounts :field "partial_accounts"
                    :translate-with #'decode-partial-account-with-avatar
                    :nullable t)
  (statuses :translate-with #'decode-status)
  (notification-groups :field "notification-groups"
                       :translate-with #'decode-notification-group))

(defun %decode-notification-policy-summary (summary-data)
    (when (hash-table-p summary-data)
      (list :pending-requests-count (gethash "pending_requests_count" summary-data)
            :pending-notifications-count (gethash "pending_notifications_count" summary-data))))

(define-entity notification-policy
  (for-not-following    :field "for_not_following")
  (for-not-followers    :field "for_not_followers")
  (for-new-accounts     :field "for_new_accounts")
  (for-private-mentions :field "for_private_mentions")
  (for-limited-accounts :field "for_limited_accounts")
  (summary :translate-with #'%decode-notification-policy-summary))

(define-entity notification-request
  (id)
  (created-at :field "created_at" :translate-with #'convert-timestamp)
  (updated-at :field "update_at"  :translate-with #'convert-timestamp)
  (account :translate-with #'decode-account)
  (notifications-count :field "notifications_count")
  (last-status :field "last_status" :translate-with #'decode-status))
