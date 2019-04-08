(** Module Date
 * Provide function manipulating dates and time. **)

(** A moment in time. **)
type t

(** Adds a given number of years to a date. **)
val add_years : t -> int -> t

(** Adds a given number of days to a date. **)
val add_days : t -> int -> t

(** Adds a given number of minutes to a date. **)
val add_minutes : t -> int -> t

(** The current date. **)
val now : t

(** The furthest possible date in the future. **)
val max_date : t

(** Compare dates. **)
val compare : t -> t -> int

(** Returns the date the furthest in the past of its two arguments. **)
val min : t -> t -> t

(** Returns the date the furthest in the future of its two arguments. **)
val max : t -> t -> t

(** Translate the date into an ISO 8601 date string (without the time). **)
val iso8601 : t -> string

(** Translate the date into an RFC 2445 date-time string. **)
val rfc2445 : t -> string

(** Translate the date into an org-mode date.
 * The optionnal argument state whether the date is supposed to be active
 * or inactive (default).
 * An active link in org-mode will be placed in the user’s agenda. **)
val orgmode : ?active:bool -> t -> string

(** Translate the date into an org-mode range.
 * Similarly to [orgmode], a facultative argument states whether the range
 * is active. **)
val orgmode_range : ?active:bool -> t -> t -> string

