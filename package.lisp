(defpackage #:tooter-objects
  (:nicknames #:org.shirakumo.tooter.objects)
  (:use)
  ;; objects.lisp
  (:export
   #:entity
   #:define-entity
   #:decode-entity
   #:account
   #:id
   #:username
   #:account-name
   #:display-name
   #:locked
   #:discoverable
   #:created-at
   #:followers-count
   #:following-count
   #:statuses-count
   #:note
   #:url
   #:avatar
   #:avatar-static
   #:header
   #:header-static
   #:moved
   #:fields
   #:bot
   #:source
   #:application
   #:name
   #:website
   #:vapid-key
   #:announcement-account
   #:id
   #:username
   #:account-name
   #:url
   #:announcement-status
   #:id
   #:url
   #:announcement
   #:id
   #:content
   #:starts-at
   #:ends-at
   #:published
   #:all-day
   #:published-at
   #:updated-at
   #:readp
   #:mentions
   #:statuses
   #:status-tag
   #:tags
   #:emojis
   #:reactions
   #:attachment
   #:id
   #:kind
   #:url
   #:remote-url
   #:preview-url
   #:text-url
   #:metadata
   #:description
   #:blurhash
   #:metadata
   #:small
   #:original
   #:focus
   #:image-metadata
   #:width
   #:height
   #:size
   #:aspect
   #:video-metadata
   #:width
   #:height
   #:frame-rate
   #:duration
   #:bitrate
   #:audio-metadata
   #:audio-length
   #:duration
   #:audio-encode
   #:audio-bitrate
   #:audio-channels
   #:card
   #:url
   #:title
   #:description
   #:image
   #:kind
   #:author-name
   #:author-url
   #:provider-name
   #:provider-url
   #:html
   #:width
   #:height
   #:image
   #:embed-url
   #:context
   #:ancestors
   #:descendants
   #:emoji
   #:shortcode
   #:url
   #:static-url
   #:visible-in-picker
   #:instance
   #:uri
   #:title
   #:description
   #:short-description
   #:email
   #:version
   #:languages
   #:registrations
   #:approval-required
   #:urls
   #:stats
   #:thumbnail
   #:contact-account
   #:instance-stats
   #:user-count
   #:status-count
   #:domain-count
   #:user-list
   #:id
   #:title
   #:marker
   #:last-read-id
   #:mention
   #:url
   #:username
   #:account
   #:id
   #:notification
   #:id
   #:kind
   #:created-at
   #:account
   #:status
   #:poll-option
   #:title
   #:votes-count
   #:poll
   #:id
   #:expires-at
   #:expired
   #:multiple
   #:voters-count
   #:votes-count
   #:voted
   #:own-votes
   #:options
   #:emojis
   #:preferences
   #:posting-default-visibility
   #:posting-default-sensitive
   #:posting-default-language
   #:reading-expand-media
   #:reading-expand-spoilers
   #:push-subscription-alerts
   #:alert-follow
   #:alert-favourite
   #:alert-mention
   #:alert-reblog
   #:alert-poll
   #:push-subscription
   #:id
   #:endpoint
   #:server-key
   #:alerts
   #:reaction
   #:name
   #:reaction-count
   #:me
   #:url
   #:static-url
   #:relationship
   #:id
   #:following
   #:requested
   #:endorsed
   #:followed-by
   #:muting
   #:muting-notifications
   #:showing-reblogs
   #:blocking
   #:domain-blocking
   #:blocked-by
   #:report
   #:id
   #:action-taken
   #:results
   #:results-accounts
   #:results-statuses
   #:results-tags
   #:status-params
   #:text
   #:in-reply-to-id
   #:media-ids
   #:sensitive
   #:spoiler-text
   #:visibility
   #:scheduled-at
   #:application-id
   #:scheduled-status
   #:id
   #:scheduled-at
   #:params
   #:status
   #:id
   #:uri
   #:url
   #:account
   #:in-reply-to-id
   #:in-reply-to-account-id
   #:parent
   #:content
   #:created-at
   #:emojis
   #:reblogs-count
   #:favourites-count
   #:replies-count
   #:reblogged
   #:favourited
   #:muted
   #:sensitive
   #:spoiler-text
   #:visibility
   #:media-attachments
   #:mentions
   #:tags
   #:application
   #:language
   #:pinned
   #:poll
   #:preview-card
   #:bookmarked
   #:source
   #:note
   #:fields
   #:privacy
   #:sensitive
   #:language
   #:follow-requests-count
   #:tag
   #:name
   #:url
   #:history
   #:tag-history
   #:day
   #:use-count
   #:account-count
   #:conversations
   #:id
   #:accounts
   #:unread
   #:last-status
   #:featured-tag
   #:id
   #:name
   #:statuses-count
   #:last-status-at
   #:filter
   #:id
   #:phrase
   #:context
   #:expires-at
   #:filter-status
   #:query-filter
   #:keyword-matches
   #:status-matches
   #:filter-keyword
   #:id
   #:status-id
   #:whole-word
   #:identity-proof
   #:provider
   #:provider-username
   #:profile-url
   #:proof-url
   #:updated-at
   #:token
   #:access-token
   #:token-type
   #:scope
   #:created-at))

(defpackage :tooter-link-header-parser
  (:use :cl)
  (:local-nicknames (:re :cl-ppcre)
                    (:a  :alexandria))
  (:export
   #:parse
   #:link-record-url
   #:link-record-parameters
   #:find-pagination-links))

(defpackage #:tooter-queries
  (:nicknames #:org.shirakumo.tooter.queries)
  (:use #:org.shirakumo.tooter.objects)
  ;; queries.lisp
  (:export
   #:navigate-page
   #:do-pages
   #:collect-all-pages
   #:get-announcements
   #:dismiss-announcement
   #:add-reaction-announcement
   #:dismiss-reaction-announcement
   #:verify-app-credentials
   #:find-account
   #:verify-credentials
   #:update-credentials
   #:get-followers
   #:get-following
   #:get-statuses
   #:bookmarks
   #:bookmark
   #:unbookmark
   #:filters
   #:filter
   #:create-filter
   #:make-update-filter-field
   #:update-filter
   #:delete-filter
   #:find-filter
   #:filter-keywords
   #:add-filter-keyword
   #:remove-filter-keyword
   #:follow
   #:unfollow
   #:block
   #:unblock
   #:mute
   #:unmute
   #:get-activity
   #:relationships
   #:search-accounts
   #:blocks
   #:blocked-domains
   #:favourites
   #:follow-requests
   #:accept-request
   #:reject-request
   #:instance
   #:peers
   #:weekly-activity
   #:emojis
   #:user-lists
   #:user-list-accounts
   #:find-list
   #:make-user-list
   #:update-user-list
   #:delete-user-list
   #:add-user-list-accounts
   #:remove-user-list-accounts
   #:make-media
   #:update-media
   #:mutes
   #:notifications
   #:find-notification
   #:delete-notification
   #:make-subscription
   #:subscription
   #:delete-subscription
   #:reports
   #:make-report
   #:find-results
   #:find-status
   #:context
   #:card
   #:rebloggers
   #:favouriters
   #:make-status
   #:delete-status
   #:edit-status
   #:reblog
   #:unreblog
   #:favourite
   #:unfavourite
   #:endorsements
   #:pin
   #:unpin
   #:mute-conversation
   #:unmute-conversation
   #:followed-tags
   #:follow-tag
   #:unfollow-tag
   #:tag-information
   #:timeline-tag
   #:timeline
   #:trends
   #:account-directory
   #:conversation
   #:delete-conversation
   #:mark-read-conversation
   #:polls
   #:poll-vote
   #:markers
   #:save-markers))

(defpackage #:tooter-client
  (:nicknames #:org.shirakumo.tooter.client)
  (:use #:org.shirakumo.tooter.objects)
  ;; client.lisp
  (:export
   #:request-failed
   #:code
   #:data
   #:message
   #:request
   #:client
   #:base
   #:key
   #:secret
   #:access-token
   #:name
   #:redirect
   #:scopes
   #:website
   #:default-headers
   #:query-url
   #:query
   #:submit
   #:register
   #:authorize)
  ;; toolkit.lisp
  (:export
   #:universal->utc-timestring
   #:convert-timestamp
   #:coerce-boolean
   #:ensure-integer
   #:to-keyword
   #:plain-format-html))

(defpackage #:tooter
  (:nicknames #:org.shirakumo.tooter)
  (:use #:cl #:tooter-objects #:tooter-queries #:tooter-client)
  (:local-nicknames (:a  :alexandria))
  (:shadowing-import-from #:tooter-queries #:block))

(dolist (package '(#:tooter-objects #:tooter-queries #:tooter-client))
  (do-symbols (symbol package)
    (export symbol '#:tooter)))
