(** Shared analytics normalization types. *)

type canonical_metric =
  | Impressions
  | Reach
  | Views
  | Engagements
  | Likes
  | Comments
  | Replies
  | Shares
  | Reposts
  | Quotes
  | Saves
  | Clicks
  | Outbound_clicks
  | Pin_clicks
  | Reactions
  | Follows
  | Followers
  | Following
  | Subscribers
  | Videos
  | Bookmarks
  | Profile_visits
  | Watch_time

let canonical_metric_key = function
  | Impressions -> "impressions"
  | Reach -> "reach"
  | Views -> "views"
  | Engagements -> "engagements"
  | Likes -> "likes"
  | Comments -> "comments"
  | Replies -> "replies"
  | Shares -> "shares"
  | Reposts -> "reposts"
  | Quotes -> "quotes"
  | Saves -> "saves"
  | Clicks -> "clicks"
  | Outbound_clicks -> "outbound_clicks"
  | Pin_clicks -> "pin_clicks"
  | Reactions -> "reactions"
  | Follows -> "follows"
  | Followers -> "followers"
  | Following -> "following"
  | Subscribers -> "subscribers"
  | Videos -> "videos"
  | Bookmarks -> "bookmarks"
  | Profile_visits -> "profile_visits"
  | Watch_time -> "watch_time"

type datapoint = {
  timestamp : string option;
  value : int;
}

type scope =
  | Account
  | Page
  | Profile
  | Post
  | Media
  | Video
  | Pin
  | Channel

let scope_key = function
  | Account -> "account"
  | Page -> "page"
  | Profile -> "profile"
  | Post -> "post"
  | Media -> "media"
  | Video -> "video"
  | Pin -> "pin"
  | Channel -> "channel"

type time_granularity =
  | Hour
  | Day
  | Week
  | Month
  | Lifetime

let time_granularity_key = function
  | Hour -> "hour"
  | Day -> "day"
  | Week -> "week"
  | Month -> "month"
  | Lifetime -> "lifetime"

type time_range = {
  since : string option;
  until_ : string option;
  granularity : time_granularity option;
}

type series = {
  metric : canonical_metric;
  scope : scope;
  provider_metric : string option;
  time_range : time_range option;
  points : datapoint list;
}

let make_datapoint ?timestamp value =
  { timestamp; value }

let make_series ?provider_metric ?time_range ~metric ~scope points =
  { metric; scope; provider_metric; time_range; points }
