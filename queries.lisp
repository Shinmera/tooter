(in-package #:org.shirakumo.tooter)

;; Announcement

(defmethod get-announcements ((client client) &key with-dismissed)
  (decode-announcement (query client "/api/v1/announcements"
                              :with-dismissed with-dismissed)))

(defmethod dismiss-announcement ((client client) (id string))
  (submit client (format nil "/api/v1/announcements/~a/dismiss" id)))

(defmethod add-reaction-announcement ((client client) (id string) (name string))
  (submit client
          (format NIL "/api/v1/announcements/~a/reactions/~a" id name)
          :http-method :put))

(defmethod dismiss-reaction-announcement ((client client) (id string) (name string))
  (submit client
          (format NIL "/api/v1/announcements/~a/reactions/~a" id name)
          :http-method :delete))

;; Application

(defmethod verify-app-credentials ((client client))
  (decode-application (query client "/api/v1/apps/verify_credentials")))

;;; Accounts

(defmethod find-account ((client client) (id string))
  (decode-account (query client (format NIL "/api/v1/accounts/~a" id))))

(defmethod verify-credentials ((client client))
  (setf (account client)
        (decode-account (query client "/api/v1/accounts/verify_credentials"))))

(defmethod update-credentials ((client client) &key display-name note avatar header (locked NIL l-p) fields)
  (check-type display-name (or null string))
  (check-type note (or null string))
  (check-type avatar (or null pathname))
  (check-type header (or null pathname))
  (check-type fields list)
  (setf (account client)
        (decode-account (apply #'submit client "/api/v1/accounts/update_credentials"
                               :display-name display-name
                               :note note
                               :avatar avatar
                               :header header
                               :locked (coerce-boolean locked l-p)
                               (loop for i from 0
                                     for (key . val) in fields
                                     collect (format NIL "fields_attributes[~a][name]" i)
                                     collect key
                                     collect (format NIL "fields_attributes[~a][value]" i)
                                     collect val)))))

(defmethod get-followers ((client client) (id string) &key max-id since-id limit)
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-account (query client (format NIL "/api/v1/accounts/~a/followers" id)
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod get-followers ((client client) (account account) &rest args)
  (apply #'get-followers client (id account) args))

(defmethod get-followers ((client client) (self (eql T)) &rest args)
  (apply #'get-followers client (id (account client)) args))

(defmethod get-following ((client client) (id string) &key max-id since-id limit)
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-account (query client (format NIL "/api/v1/accounts/~a/following" id)
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod get-following ((client client) (account account) &rest args)
  (apply #'get-following client (id account) args))

(defmethod get-following ((client client) (self (eql T)) &rest args)
  (apply #'get-following client (id (account client)) args))

(defmethod get-statuses ((client client) (id string) &key (only-media NIL o-p) (pinned NIL p-p) (exclude-replies NIL e-p) max-id since-id limit)
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-status (query client (format NIL "/api/v1/accounts/~a/statuses" id)
                        :only-media (coerce-boolean only-media o-p)
                        :pinned (coerce-boolean pinned p-p)
                        :exclude-replies (coerce-boolean exclude-replies e-p)
                        :max-id max-id
                        :since-id since-id
                        :limit limit)))

(defmethod get-statuses ((client client) (account account) &rest args)
  (apply #'get-statuses client (id account) args))

(defmethod get-statuses ((client client) (self (eql T)) &rest args)
  (apply #'get-statuses client (id (account client)) args))

;;; Bookmarks

(defmethod bookmarks ((client client) &key max-id since-id min-id (limit 20))
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type min-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-status (query client "/api/v1/bookmarks"
                        :max-id max-id
                        :since-id since-id
                        :min-id min-id
                        :limit limit)))

(defmethod bookmark ((client client) (id string))
  (check-type id string)
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/bookmark" id))))

(defmethod bookmark ((client client) (status status))
  (bookmark client (id status)))

(defmethod unbookmark ((client client) (id string))
  (check-type id string)
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/unbookmark" id))))

(defmethod unbookmark ((client client) (status status))
  (unbookmark client (id status)))

;;; Filters

(defmethod filters ((client client))
  (decode-filter (query client "/api/v1/filters")))

(defmethod filter ((client client) (id string))
  (decode-filter (query client (format NIL "/api/v1/filters/~a" id))))

(defmethod filter ((client client) (filter filter))
  (filter client (id filter)))

(defmethod create-filter ((client client) phrase context &key (irreversible NIL i-p) (whole-word NIL w-p) expires-in)
  (assert (stringp phrase))
  (assert (consp context))
  (decode-filter (submit client "/api/v1/filters"
                         :phrase  phrase
                         :context context
                         :irreversible (coerce-boolean irreversible i-p)
                         :whole-word (coerce-boolean whole-word w-p)
                         :expires-in expires-in)))

(defmethod update-filter ((client client) id phrase context &key (irreversible NIL i-p) (whole-word NIL w-p) expires-in)
  (assert (stringp id))
  (assert (stringp phrase))
  (decode-filter (submit client (format NIL "/api/v1/filters/~a" id)
                         :http-method :put
                         :phrase  phrase
                         :context context
                         :irreversible (coerce-boolean irreversible i-p)
                         :whole-word (coerce-boolean whole-word w-p)
                         :expires-in expires-in)))

(defmethod delete-filter ((client client) id)
  (assert (stringp id))
  (decode-filter (submit client (format NIL "/api/v1/filters/~a" id)
                         :http-method :delete)))


;;; Follows

(defmethod follow ((client client) (id string))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/follow" id))))

(defmethod follow ((client client) (account account))
  (follow client (id account)))

(defmethod unfollow ((client client) (id string))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/unfollow" id))))

(defmethod unfollow ((client client) (account account))
  (unfollow client (id account)))

(defmethod block ((client client) (id string))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/block" id))))

(defmethod block ((client client) (account account))
  (block client (id account)))

(defmethod unblock ((client client) (id string))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/unblock" id))))

(defmethod unblock ((client client) (account account))
  (unblock client (id account)))

(defmethod mute ((client client) (id string) &key (notifications T n-p))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/mute" id)
                               :notifications (coerce-boolean notifications n-p))))

(defmethod mute ((client client) (account account) &rest args)
  (apply #'mute client (id account) args))

(defmethod unmute ((client client) (id string))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/unmute" id))))

(defmethod unmute ((client client) (account account))
  (unblock client (id account)))

(defmethod get-activity ((client client))
  (decode-activity (query client "/api/v1/instance/activity")))

(defmethod relationships ((client client) (ids cons))
  (decode-relationship (query client "/api/v1/accounts/relationships"
                              :id (loop for id in ids
                                        collect (etypecase id
                                                  (account (id id))
                                                  (string id))))))

(defmethod relationships ((client client) (account account))
  (relationships client (list (id account))))

(defmethod search-accounts ((client client) query &key (limit 40) (following NIL f-p) (resolve NIL r-p))
  (check-type query string)
  (check-type limit (or null (integer 0)))
  (decode-account (query client "/api/v1/accounts/search"
                         :q query
                         :limit limit
                         :following (coerce-boolean following f-p)
                         :resolve   (coerce-boolean resolve r-p))))

;;; Blocks

(defmethod blocks ((client client) &key max-id since-id (limit 40))
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-account (query client "/api/v1/blocks"
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod blocked-domains ((client client) &key max-id since-id (limit 40))
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (query client "/api/v1/domain_blocks"
         :max-id max-id
         :since-id since-id
         :limit limit))

(defmethod block ((client client) (domain string))
  (submit client "/api/v1/domain_blocks"
          :domain domain)
  T)

(defmethod unblock ((client client) (domain string))
  (submit client "/api/v1/domain_blocks"
          :http-method :delete
          :domain domain)
  T)

;;; Favourites

(defmethod favourites ((client client) &key min-id max-id (limit 20))
  (check-type max-id (or null string))
  (check-type min-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-status (query client "/api/v1/favourites"
                        :min-id min-id
                        :max-id max-id
                        :limit limit)))

;;; Follow Requests

(defmethod follow-requests ((client client) &key max-id since-id (limit 40))
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-account (query client "/api/v1/follow_requests"
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod accept-request ((client client) (id string))
  (decode-relationship (submit client (format NIL "/api/v1/follow_requests/~a/authorize" id))))

(defmethod accept-request ((client client) (account account))
  (accept-request client (id account)))

(defmethod reject-request ((client client) (id string))
  (decode-relationship (submit client (format NIL "/api/v1/follow_requests/~a/reject" id))))

(defmethod reject-request ((client client) (account account))
  (reject-request client (id account)))

;;; Instances

(defmethod instance ((client client))
  (decode-instance (query client "/api/v1/instance")))

(defmethod peers ((client client))
  (query client "/api/v1/instance/peers"))

(defmethod weekly-activity ((client client))
  (decode-activity (query client "/api/v1/instance/activity")))

(defmethod emojis ((client client))
  (decode-emoji (query client "/api/v1/custom_emojis")))

;;; Lists

(defmethod user-lists ((client client) (id string))
  (decode-user-list (query client (format NIL "/api/v1/accounts/~a/lists" id))))

(defmethod user-lists ((client client) (account account))
  (user-lists client (id account)))

(defmethod user-lists ((client client) (id (eql T)))
  (decode-user-list (query client "/api/v1/lists")))

(defmethod user-list-accounts ((client client) (id string) &key max-id since-id limit)
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-account (query client (format NIL "/api/v1/lists/~a/accounts" id)
                         :max-id max-id
                         :since-id since-id
                         :limit (or limit 0))))

(defmethod user-list-accounts ((client client) (user-list user-list) &rest args)
  (apply #'user-list-accounts client (id user-list) args))

;; Convenience.
(defmethod (setf user-list-accounts) (accounts (client client) (id string))
  (let* ((accounts (loop for account in accounts
                         collect (etypecase account
                                   (string account)
                                   (account (id account)))))
         (existing (mapcar #'id (user-list-accounts client id)))
         (to-remove (set-difference existing accounts))
         (to-add (set-difference accounts existing)))
    (when to-remove (remove-user-list-accounts client id to-remove))
    (when to-add (add-user-list-accounts client id to-add))
    accounts))

(defmethod find-list ((client client) (id string))
  (decode-user-list (query client (format NIL "/api/v1/lists/~a" id))))

(defmethod make-user-list ((client client) title)
  (decode-user-list (submit client "/api/v1/lists"
                            :title title)))

(defmethod update-user-list ((client client) (id string) &key title)
  (check-type title string)
  (decode-user-list (submit client (format NIL "/api/v1/lists/~a" id)
                            :http-method :put
                            :title title)))

(defmethod update-user-list ((client client) (user-list user-list) &rest args)
  (apply #'update-user-list client (id user-list) args))

(defmethod delete-user-list ((client client) (id string))
  (submit client (format NIL "/api/v1/lists/~a" id)
          :http-method :delete)
  T)

(defmethod add-user-list-accounts ((client client) (id string) accounts)
  (submit client (format NIL "/api/v1/lists/~a/accounts" id)
          :account-ids (loop for account in accounts
                             collect (etypecase account
                                       (string account)
                                       (account (id account)))))
  T)

(defmethod add-user-list-accounts ((client client) (user-list user-list) accounts)
  (add-user-list-accounts client (id user-list) accounts))

(defmethod remove-user-list-accounts ((client client) (id string) accounts)
  (submit client (format NIL "/api/v1/lists/~a/accounts" id)
          :http-method :delete
          :account-ids (loop for account in accounts
                             collect (etypecase account
                                       (string account)
                                       (account (id account)))))
  T)

(defmethod remove-user-list-accounts ((client client) (user-list user-list) accounts)
  (remove-user-list-accounts client (id user-list) accounts))

;;; Media

(defmethod make-media ((client client) file &key description focus)
  (check-type file pathname)
  (check-type description (or null string))
  (check-type focus (or null cons))
  (decode-attachment (submit client "/api/v1/media"
                             :file file
                             :description description
                             :focus (when focus (format NIL "~f,~f" (car focus) (cdr focus))))))

(defmethod update-media ((client client) id &key description focus)
  (check-type description (or null string))
  (check-type focus (or null cons))
  (decode-attachment (submit client (format NIL "/api/v1/media/~a" id)
                             :description description
                             :focus (when focus (format NIL "~f,~f" (car focus) (cdr focus))))))

(defmethod update-media ((client client) (attachment attachment) &rest args)
  (apply #'update-media client (id attachment) args))

;;; Mutes

(defmethod mutes ((client client) &key max-id since-id (limit 40))
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  ;; FIXME Link headers
  (decode-account (query client "/api/v1/mutes"
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

;;; Notifications

(defun encode-notification-type (encoded-type)
  (ecase encoded-type
    (:follow "follow")
    (:favourite "favourite")
    (:reblog "reblog")
    (:mention "mention")
    (:move "move")
    (:poll "poll")
    (:follow-request "follow_request")))

(defmethod notifications ((client client) &key max-id min-id since-id (limit 15) exclude-types account-id)
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (check-type exclude-types list)
  (decode-notification (query client "/api/v1/notifications"
                              :max-id max-id
                              :min-id min-id
                              :since-id since-id
                              :limit limit
                              :exclude-types (loop for type in exclude-types
                                                   collect (encode-notification-type type))
                              :account-id account-id)))

(defmethod find-notification ((client client) (id string))
  (decode-notification (query client (format NIL "/api/v1/notifications/~a" id))))

(defmethod delete-notification ((client client) (all (eql T)))
  (submit client "/api/v1/notifications/clear")
  T)

(defmethod delete-notification ((client client) (id string))
  (submit client (format nil "/api/v1/notifications/~a/dismiss" id))
  T)

(defmethod delete-notification ((client client) (notification notification))
  (delete-notification client (id notification)))

(defmethod make-subscription ((client client) endpoint public-key secret &key alerts)
  (check-type endpoint string)
  (check-type public-key string)
  (check-type secret string)
  (check-type alerts list)
  (decode-push-subscription (apply #'submit client "/api/v1/push/subscription"
                                   "subscription[endpoint]" endpoint
                                   "subscription[keys][p256dh]" public-key
                                   "subscription[keys][auth]" secret
                                   (loop for alert in alerts
                                         collect (ecase alert
                                                   (:follows "data[alerts][follow]")
                                                   (:favourites "data[alerts][favourite]")
                                                   (:reblogs "data[alerts][reblogs]")
                                                   (:mentions "data[alerts][mention]")
                                                   (:polls "data[alerts][poll]"))
                                         collect "true"))))

(defmethod subscription ((client client))
  (decode-push-subscription (query client "/api/v1/push/subscription")))

(defmethod delete-subscription ((client client))
  (submit client "/api/v1/push/subscription"
          :http-method :delete)
  T)

;;; Reports

(defmethod reports ((client client))
  (decode-report (query client "/api/v1/reports")))

(defmethod make-report ((client client) (id string) &key statuses comment forward)
  (check-type statuses list)
  (check-type comment (or null string))
  (decode-report (submit client "/api/v1/reports"
                         :account-id id
                         :status-ids (loop for status in statuses
                                           collect (etypecase status
                                                     (string status)
                                                     (status (id status))))
                         :comment comment
                         :forward (coerce-boolean forward t))))

(defmethod make-report ((client client) (account account) &rest args)
  (apply #'make-report client (id account) args))

;;; Search

(defmethod find-results ((client client) query &key account-id max-id min-id kind (exclude-unreviewed NIL e-p) (resolve NIL r-p) (limit 20) (offset 0) (following NIL f-p))
  (check-type account-id (or null string))
  (check-type max-id (or null string))
  (check-type min-id (or null string))
  (check-type kind (or null string))
  (decode-results (query client "/api/v2/search"
                         :q query
                         :account-id account-id
                         :max-id max-id
                         :min-id min-id
                         :type   kind
                         :exclude-unreviewed (coerce-boolean exclude-unreviewed e-p)
                         :resolve (coerce-boolean resolve r-p)
                         :limit limit
                         :offset offset
                         :following (coerce-boolean following f-p))))

;;; Statuses

(defmethod find-status ((client client) (id string))
  (decode-status (query client (format NIL "/api/v1/statuses/~a" id))))

(defmethod context ((client client) (id string))
  (decode-context (query client (format NIL "/api/v1/statuses/~a/context" id))))

(defmethod context ((client client) (status status))
  (context client (id status)))

(defmethod card ((client client) (id string))
  (decode-card (query client (format NIL "/api/v1/statuses/~a/context" id))))

(defmethod card ((client client) (status status))
  (card client (id status)))

(defmethod rebloggers ((client client) (id string) &key max-id since-id (limit 40))
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-account (query client (format NIL "/api/v1/statuses/~a/reblogged_by" id)
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod rebloggers ((client client) (status status) &rest args)
  (apply #'rebloggers client (id status) args))

(defmethod favouriters ((client client) (id string) &key max-id since-id (limit 40))
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-account (query client (format NIL "/api/v1/statuses/~a/favourited_by" id)
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod favouriters ((client client) (status status) &rest args)
  (apply #'favouriters client (id status) args))

(defmethod make-status ((client client) status &key in-reply-to media (sensitive NIL s-p) spoiler-text visibility language scheduled-at poll-options poll-expire-seconds (poll-multiple NIL m-p) (poll-hide-totals NIL h-p) idempotency-key)
  (flet ((ensure-media-id (media)
           (etypecase media
             (string media)
             (attachment (id media))
             (pathname (id (make-media client media))))))
    (let ((results-entity (submit client "/api/v1/statuses"
                                  :status status
                                  :in-reply-to-id (etypecase in-reply-to
                                                    (string in-reply-to)
                                                    (status (id in-reply-to))
                                                    (null NIL))
                                  :media-ids (typecase media
                                               (null NIL)
                                               (cons (mapcar #'ensure-media-id media))
                                               (T (list (ensure-media-id media))))
                                  :sensitive (coerce-boolean sensitive s-p)
                                  :spoiler-text spoiler-text
                                  :visibility (ecase visibility
                                                ((NIL) NIL)
                                                (:direct "direct")
                                                (:private "private")
                                                (:unlisted "unlisted")
                                                (:public "public"))
                                  :language (when language (string language))
                                  :scheduled-at scheduled-at
                                  "poll[options]" poll-options
                                  "poll[expires-in]" poll-expire-seconds
                                  "poll[multiple]" (coerce-boolean poll-multiple m-p)
                                  "poll[hide_totals]" (coerce-boolean poll-hide-totals h-p)
                                  :idempotency-key idempotency-key)))
      (if scheduled-at
          (decode-scheduled-status results-entity)
          (decode-status results-entity)))))

(defmethod delete-status ((client client) (id string))
  (submit client (format NIL "/api/v1/statuses/~a" id)
          :http-method :delete)
  T)

(defmethod delete-status ((client client) (status status))
  (delete-status client (id status)))

(defmethod reblog ((client client) (id string))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/reblog" id))))

(defmethod reblog ((client client) (status status))
  (reblog client (id status)))

(defmethod unreblog ((client client) (id string))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/unreblog" id))))

(defmethod unreblog ((client client) (status status))
  (unreblog client (id status)))

(defmethod favourite ((client client) (id string))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/favourite" id))))

(defmethod favourite ((client client) (status status))
  (favourite client (id status)))

(defmethod unfavourite ((client client) (id string))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/unfavourite" id))))

(defmethod unfavourite ((client client) (status status))
  (unfavourite client (id status)))

(defmethod endorsements ((client client))
  (decode-account (query client "/api/v1/endorsements")))

(defmethod pin ((client client) (id string))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/pin" id))))

(defmethod pin ((client client) (status status))
  (pin client (id status)))

(defmethod unpin ((client client) (id string))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/unpin" id))))

(defmethod unpin ((client client) (status status))
  (unpin client (id status)))

(defmethod mute-conversation ((client client) (id string))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/mute" id))))

(defmethod mute-conversation ((client client) (status status))
  (mute-conversation client (id status)))

(defmethod unmute-conversation ((client client) (id string))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/unmute" id))))

(defmethod unmute-conversation ((client client) (status status))
  (mute-conversation client (id status)))

(defmethod mute ((client client) (status status) &key)
  (mute-conversation client (id status)))

(defmethod unmute ((client client) (status status))
  (unmute-conversation client (id status)))

;;; Timelines
(defun %timeline (client url &key (local NIL l-p) (only-media NIL o-p) max-id since-id min-id (limit 20))
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type min-id (or null string))
  (check-type limit (or null (integer 0)))
  (decode-status (query client (format NIL "/api/v1/timelines/~a" url)
                        :local (coerce-boolean local l-p)
                        :only-media (coerce-boolean only-media o-p)
                        :max-id max-id
                        :since-id since-id
                        :min-id min-id
                        :limit limit)))

(defgeneric timeline-tag (client tag &rest args))

(defmethod timeline-tag ((client client) (tag string) &rest args)
  (apply #'%timeline client (format NIL "tag/~a" tag) args))

(defgeneric timeline (client kind &key local only-media max-id since-id limit min-id))

(defmethod timeline ((client client) (kind (eql :home)) &rest args)
  (apply #'%timeline client "home" args))

(defmethod timeline ((client client) (kind (eql :public)) &rest args)
  (apply #'%timeline client "public" args))

(defmethod timeline ((client client) (id string) &rest args)
  (apply #'%timeline client (format NIL "list/~a" id) args))

(defmethod timeline ((client client) (tag tag) &rest args)
  (apply #'timeline-tag client (name tag) args))

(defmethod timeline ((client client) (user-list user-list) &rest args)
  (apply #'timeline client (id user-list) args))

;;; Trends

(defmethod trends ((client client))
  (decode-tag (query client "/api/v1/trends")))

;;; Directory

(defmethod account-directory ((client client))
  (decode-account (query client "/api/v1/directory")))

;;; Conversations

(defgeneric conversations (client &key limit max-id since-id min-id))

(defmethod conversations ((client client) &key (limit 20) max-id since-id min-id)
  (decode-conversation (query client "/api/v1/conversations"
                              :max-id max-id
                              :since-id since-id
                              :min-id min-id
                              :limit limit)))

(defmethod delete-conversation ((client client) (id string))
  (query client
         (format NIL "/api/v1/conversations/~a" id)
         :http-method :delete))

(defmethod mark-read-conversation ((client client) (id string))
  (decode-conversation (submit client
                               (format NIL "/api/v1/conversations/~a/read" id))))

;;; Polls

(defmethod polls ((client client) (id string))
  (decode-poll (query client (format NIL "/api/v1/polls/~a" id))))

(defmethod polls ((client client) (poll poll))
  (polls client (id poll)))

(defmethod poll-vote ((client client) (id string) (choices list))
  (assert (every #'stringp choices))
  (assert (every (lambda (a) (parse-integer a :junk-allowed t)) choices))
  (decode-poll (submit client (format NIL "/api/v1/polls/~a/votes" id)
                       "choices" choices)))

(defmethod poll-vote ((client client) (poll poll) choices)
  (poll-vote client (id poll) choices))

;; Preferences

(defmethod preferences ((client client))
  (decode-preferences (query client "/api/v1/preferences")))

;;; Markers

(defmethod markers ((client client) (timeline list))
  (decode-marker (query client "/api/v1/markers/"
                        :timeline timeline)))

(defmethod save-markers ((client client) &key last-status-read last-notification-read)
 (decode-marker (submit client "/api/v1/markers/"
                        "home[last_read_id]" last-status-read
                        "notifications[last_read_id]" last-notification-read)))

;;; Identity proof

(defmethod identity-proof ((client client) (provider string) (username string))
   (query client "/api/proofs"
          :provider provider
          :username username))

(defmethod oembed ((client client) (url string) &key (max-width 400) max-height)
  (query client "/api/oembed"
         :url url
         :maxwidth max-width
         :maxheight max-height))
