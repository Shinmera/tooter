#|
 This file is a part of Tooter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:tooter-objects
  (:nicknames #:org.shirakumo.tooter.objects)
  (:use)
  ;; objects.lisp
  (:export
   #:entity
   #:define-entity
   #:decode-entity
   #:field
   #:decode-field
   #:account
   #:decode-account
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
   #:activity
   #:decode-activity
   #:application
   #:name
   #:website
   #:vapid-key
   #:decode-application
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
   #:decode-attachment
   #:metadata
   #:small
   #:original
   #:focus
   #:decode-metadata
   #:image-metadata
   #:width
   #:height
   #:size
   #:aspect
   #:decode-image-metadata
   #:video-metadata
   #:width
   #:height
   #:frame-rate
   #:duration
   #:bitrate
   #:decode-video-metadata
   #:audio-metadata
   #:audio-length
   #:duration
   #:audio-encode
   #:audio-bitrate
   #:audio-channels
   #:decode-audio-metadata
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
   #:decode-card
   #:context
   #:ancestors
   #:descendants
   #:decode-context
   #:emoji
   #:shortcode
   #:url
   #:static-url
   #:visible-in-picker
   #:category
   #:decode-emoji
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
   #:decode-instance
   #:instance-stats
   #:user-count
   #:status-count
   #:domain-count
   #:decode-instance-stats
   #:user-list
   #:id
   #:title
   #:decode-user-list
   #:marker
   #:marked-home
   #:marked-notifications
   #:decode-marker
   #:mention
   #:url
   #:username
   #:account
   #:id
   #:decode-mention
   #:notification
   #:id
   #:kind
   #:created-at
   #:account
   #:status
   #:decode-notification
   #:poll-option
   #:title
   #:votes-count
   #:decode-poll-option
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
   #:decode-poll
   #:preferences
   #:posting-default-visibility
   #:posting-default-sensitive
   #:posting-default-language
   #:reading-expand-media
   #:reading-expand-spoilers
   #:decode-preferences
   #:push-subscription-alerts
   #:alert-follow
   #:alert-favourite
   #:alert-mention
   #:alert-reblog
   #:alert-poll
   #:decode-push-subscription-alerts
   #:push-subscription
   #:id
   #:endpoint
   #:server-key
   #:alerts
   #:decode-push-subscription
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
   #:decode-relationship
   #:report
   #:id
   #:action-taken
   #:decode-report
   #:results
   #:results-accounts
   #:results-statuses
   #:hashtags
   #:decode-results
   #:status-params
   #:text
   #:in-reply-to-id
   #:media-ids
   #:sensitive
   #:spoiler-text
   #:visibility
   #:scheduled-at
   #:application-id
   #:decode-status-params
   #:scheduled-status
   #:id
   #:scheduled-at
   #:params
   #:decode-scheduled-status
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
   #:decode-status
   #:source
   #:note
   #:fields
   #:privacy
   #:sensitive
   #:language
   #:follow-requests-count
   #:decode-source
   #:tag
   #:name
   #:url
   #:history
   #:decode-tag
   #:tag-history
   #:day
   #:use-count
   #:account-count
   #:decode-tag-history
   #:conversations
   #:id
   #:accounts
   #:unread
   #:last-status
   #:decode-conversation
   #:featured-tag
   #:id
   #:name
   #:statuses-count
   #:last-status-at
   #:decode-featured-tag
   #:filter
   #:id
   #:phrase
   #:context
   #:expires-at
   #:irreversible
   #:whole-word
   #:decode-filter
   #:identity-proof
   #:provider
   #:provider-username
   #:profile-url
   #:proof-url
   #:updated-at
   #:decode-identity-proof
   #:token
   #:access-token
   #:token-type
   #:scope
   #:created-at
   #:decode-token))

(defpackage #:tooter-queries
  (:nicknames #:org.shirakumo.tooter.queries)
  (:use #:org.shirakumo.tooter.objects)
  ;; queries.lisp
  (:export
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
   #:update-filter
   #:delete-filter
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
   #:follow-remote
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
   #:reblog
   #:unreblog
   #:favourite
   #:unfavourite
   #:endorsements
   #:pin
   #:unpin
   #:mute-conversation
   #:unmute-conversation
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
   #:query
   #:submit
   #:register
   #:authorize)
  ;; toolkit.lisp
  (:export
   #:universal->utc-timestring
   #:convert-timestamp
   #:plain-format-html))

(defpackage #:tooter
  (:nicknames #:org.shirakumo.tooter)
  (:use #:cl #:tooter-objects #:tooter-queries #:tooter-client)
  (:shadowing-import-from #:tooter-queries #:block))

(dolist (package '(#:tooter-objects #:tooter-queries #:tooter-client))
  (do-symbols (symbol package)
    (export symbol '#:tooter)))
