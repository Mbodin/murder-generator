(** Module Date
 * Provide function manipulating dates and time. **)

(** A moment in time. **)
type t

(** Add a given number of years to a date. **)
val add_years : t -> int -> t

(** Add a given number of days to a date. **)
val add_days : t -> int -> t

(** Add a given number of minutes to a date. **)
val add_minutes : t -> int -> t

(** The current date. **)
val now : t

(** The furthest possible date in the future. **)
val max_date : t

(** Compare dates. **)
val compare : t -> t -> int

(** Return the date the furthest in the past of its two arguments. **)
val min : t -> t -> t

(** Return the date the furthest in the future of its two arguments. **)
val max : t -> t -> t

(** Return the year associated with this date. **)
val year : t -> int

(** Return the month associated with this date, as an integer from [1] to [12]. **)
val month : t -> int

(** Return the day of the month associated with this date, as an integer from [1] to [31]. **)
val day : t -> int

(** Return the hour of the day associated with this date, as an integer from [0] to [23]. **)
val hour : t -> int

(** Return the minute of the current hour associated with this date, as an integer from [0] to [59]. **)
val minute : t -> int

(** Translate the date into an ISO 8601 date string (without the time). **)
val iso8601 : t -> string

(** Invert the function [iso8601].
 * Note that the time of the day will not be recovered. **)
val from_iso8601 : string -> t

(** Translate the date into an RFC 2445 date-time string. **)
val rfc2445 : t -> string

(** Invert the function [rfc2445]. **)
val from_rfc2445 : string -> t

(** Translate the date into the date format used by Timeline. **)
val timeline : t -> string

(** Translate the date into an org-mode date.
 * The optionnal argument state whether the date is supposed to be active
 * or inactive (default).
 * An active link in org-mode will be placed in the user’s agenda. **)
val orgmode : ?active:bool -> t -> string

(** Translate the date into an org-mode range.
 * Similarly to [orgmode], a facultative argument states whether the range
 * is active. **)
val orgmode_range : ?active:bool -> t -> t -> string

