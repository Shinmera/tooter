#|
 This file is a part of Tooter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.tooter)

;; client.lisp
(docs:define-docs
  (type request-failed
    "")

  (function code
    "")

  (function data
    "")

  (function message
    "")

  (function request
    "")

  (type client
    "")

  (function base
    "")

  (function key
    "")

  (function secret
    "")

  (function access-token
    "")

  (function name
    "")

  (function redirect
    "")

  (function scopes
    "")

  (function website
    "")

  (function default-headers
    "")

  (function query
    "")

  (function submit
    "")

  (function register
    "")

  (function authorize
    ""))

;; objects.lisp
(docs:define-docs
  (type entity
    "")

  (function decode-entity
    "")

  (type account
    "")

  (function id
    "")

  (function username
    "")

  (function display-name
    "")

  (function locked
    "")

  (function created-at
    "")

  (function followers-count
    "")

  (function following-count
    "")

  (function statuses-count
    "")

  (function note
    "")

  (function url
    "")

  (function avatar
    "")

  (function avatar-static
    "")

  (function header
    "")

  (function header-static
    "")

  (function moved
    "")

  (function fields
    "")

  (function bot
    "")

  (function source
    "")

  (type application
    "")

  (function name
    "")

  (function website
    "")

  (type attachment
    "")

  (function kind
    "")

  (function remote-url
    "")

  (function preview-url
    "")

  (function text-url
    "")

  (type metadata
    "")

  (function description
    "")

  (function small
    "")

  (function original
    "")

  (function focus
    "")

  (type image-metadata
    "")

  (function width
    "")

  (function height
    "")

  (function size
    "")

  (function aspect
    "")

  (type video-metadata
    "")

  (function frame-rate
    "")

  (function duration
    "")

  (function bitrate
    "")

  (type card
    "")

  (function title
    "")

  (function image
    "")

  (function author-name
    "")

  (function author-url
    "")

  (function provider-name
    "")

  (function provider-url
    "")

  (function html
    "")

  (type context
    "")

  (function ancestors
    "")

  (function descendants
    "")

  (type emoji
    "")

  (function shortcode
    "")

  (function static-url
    "")

  (type instance
    "")

  (function uri
    "")

  (function email
    "")

  (function version
    "")

  (function urls
    "")

  (function languages
    "")

  (function contact-account
    "")

  (type user-list
    "")

  (type mention
    "")

  (type notification
    "")

  (function status
    "")

  (type push-subscription
    "")

  (function endpoint
    "")

  (function server-key
    "")

  (function alerts
    "")

  (type relationship
    "")

  (function following
    "")

  (function followers
    "")

  (function blocking
    "")

  (function muting
    "")

  (function muting-notifications
    "")

  (function requested
    "")

  (function domain-blocking
    "")

  (type report
    "")

  (function action-taken
    "")

  (type results
    "")

  (function accounts
    "")

  (function statuses
    "")

  (function hashtags
    "")

  (type status
    "")

  (function in-reply-to-id
    "")

  (function in-reply-to-account-id
    "")

  (function parent
    "")

  (function content
    "")

  (function emojis
    "")

  (function reblogs-count
    "")

  (function favourites-count
    "")

  (function reblogged
    "")

  (function favourited
    "")

  (function muted
    "")

  (function sensitive
    "")

  (function spoiler-text
    "")

  (function visibility
    "")

  (function media-attachments
    "")

  (function mentions
    "")

  (function tags
    "")

  (function language
    "")

  (function pinned
    "")

  (type tag
    "")

  (function history
    "")

  (type tag-history
    "")

  (function day
    "")

  (function uses
    "")

  (function accounts
    ""))

;; queries.lisp
(docs:define-docs
  (function find-account
    "")

  (function verify-credentials
    "")

  (function update-credentials
    "")

  (function get-followers
    "")

  (function get-following
    "")

  (function get-statuses
    "")

  (function follow
    "")

  (function unfollow
    "")

  (function block
    "")

  (function unblock
    "")

  (function mute
    "")

  (function unmute
    "")

  (function relationships
    "")

  (function search-accounts
    "")

  (function blocks
    "")

  (function blocked-domains
    "")

  (function favourites
    "")

  (function follow-requests
    "")

  (function accept-request
    "")

  (function reject-request
    "")

  (function follow-remote
    "")

  (function instance
    "")

  (function emojis
    "")

  (function user-lists
    "")

  (function user-list-accounts
    "")

  (function find-list
    "")

  (function make-user-list
    "")

  (function update-user-list
    "")

  (function delete-user-list
    "")

  (function add-user-list-accounts
    "")

  (function remove-user-list-accounts
    "")

  (function make-media
    "")

  (function update-media
    "")

  (function mutes
    "")

  (function notifications
    "")

  (function find-notification
    "")

  (function delete-notification
    "")

  (function make-subscription
    "")

  (function subscription
    "")

  (function delete-subscription
    "")

  (function reports
    "")

  (function report
    "")

  (function results
    "")

  (function find-status
    "")

  (function context
    "")

  (function card
    "")

  (function rebloggers
    "")

  (function favouriters
    "")

  (function make-status
    "")

  (function delete-status
    "")

  (function reblog
    "")

  (function unreblog
    "")

  (function favourite
    "")

  (function unfavourite
    "")

  (function pin
    "")

  (function unpin
    "")

  (function mute-conversation
    "")

  (function timeline
    "")

  (function trends
    ""))

;; toolkit.lisp
(docs:define-docs
  (function plain-format-html
    ""))
