open Tyxml_html

module Verb : sig
  type t =
    [ `Delete of Uri.t
    | `Get of Uri.t
    | `Patch of Uri.t
    | `Post of Uri.t
    | `Put of Uri.t
    ]

  val to_attr : t -> 'a attrib
  val to_string : t -> string
  val uri : t -> Uri.t
  val uri_to_string : t -> string
  val verb_to_string : t -> string
end

module Css : sig
  type t =
    [ `Added
      (** Applied to a new piece of content before it is swapped, removed after it is settled. *)
    | `Indicator
      (** A dynamically generated class that will toggle visible (opacity:1) when a htmx-request class is present *)
    | `Request
      (** Applied to either the element or the element specified with hx-indicator while a request is ongoing *)
    | `Settling
      (** Applied to a target after content is swapped, removed after it is settled. The duration can be modified via hx-swap. *)
    | `Swapping
      (** Applied to a target before any content is swapped, removed after it is swapped. The duration can be modified via hx-swap. *)
    ]

  val to_string : t -> string

  module Selector : sig
    type t

    val of_string : string -> t
    val to_string : t -> string
  end
end

module Swap : sig
  module Modifier : sig
    type scroll_behaviour =
      { direction : [ `Top | `Bottom ]
      ; selector : Css.Selector.t option
      }

    val make_scroll_behaviour
      :  ?selector:Css.Selector.t
      -> [ `Top | `Bottom ]
      -> scroll_behaviour

    val scroll_behaviour_to_string : scroll_behaviour -> string

    type t =
      [ `Transition of bool
        (** Enables use of the View Transitions API when a swap occurs *)
      | `Swap of [ `Seconds of int | `Milliseconds of int ]
        (** Sets the duration of the swap animation *)
      | `Settle of [ `Seconds of int | `Milliseconds of int ]
        (** Sets the duration of the settle animation *)
      | `Scroll of scroll_behaviour
        (** Controls scrolling behavior: sets viewport to target's top/bottom,
            a specific element's top/bottom using a CSS selector, or window's top/bottom. *)
      | `Show of scroll_behaviour
        (** Adjusts visibility: ensures target's top/bottom, a specific element's
            top/bottom via a CSS selector, or window's top/bottom is in view. *)
      | `Focus_scrolls of bool
        (** Manages focus preservation; `focus-scroll:true` auto-scrolls to
            focused input, `focus-scroll:false` disables it. *)
      ]

    val to_string : t -> string
  end

  type strategy =
    [ `InnerHTML
      (** The default, replace the inner html of the target element *)
    | `OuterHTML
      (** Replace the entire target element with the response *)
    | `BeforeBegin
      (** Insert the response before the target element *)
    | `AfterBegin
      (** Insert the response before the first child of the target element *)
    | `BeforeEnd
      (** Insert the response after the last child of the target element *)
    | `AfterEnd
      (** Insert the response after the target element *)
    | `Delete
      (** Deletes the target element regardless of the response *)
    | `None
    ]

  val strategy_to_string : strategy -> string

  type t =
    { strategy : strategy
    ; modifier : Modifier.t option
    }
end

module Target : sig
  type t =
    [ `This
      (** Indicates that the element that the hx-target attribute is on is the target *)
    | `Closest of Css.Selector.t
      (** The closest ancestor element or itself, that matches the given CSS
          selector (e.g. closest tr will target the closest table row to the element) *)
    | `Css_selector of Css.Selector.t
      (** A CSS query selector of the element to target *)
    | `Find of Css.Selector.t
      (** The first child descendant element that matches the given CSS selector *)
    | `Previous of Css.Selector.t
      (** Scan the DOM backwards for the first element that matches the given CSS
          selector. (e.g previous .error will target the closest previous sibling
          with error class *)
    ]

  val to_string : t -> string
end

module Trigger : sig
  module Event : sig
    (* As we need/use more Html events, add them here. *)
    type focus_event =
      [ `Focus
      | `Focus_in
      | `Focus_out
      | `Blur
      ]

    type keyboard_event =
      [ `Key_down
      | `Key_press
      | `Key_up
      ]

    type mouse_event =
      [ `Mouse_enter
      | `Mouse_leave
      | `Mouse_move
      | `Mouse_out
      | `Mouse_over
      | `Mouse_up
      | `Mouse_wheel
      ]

    type natural_event =
      [ `Change
      | `Click
      | `Submit
      ]

    type t =
      [ focus_event
      | keyboard_event
      | natural_event
      | `Custom of string
      ]

    val to_string : t -> string
  end

  module Modifier : sig
    type standard_modifier =
      [ `Once
        (** The event will only trigger once (e.g. the first click) *)
      | `Changed
        (** The event will only change if the value of the element has changed *)
      | `Delay of [ `Seconds of int | `Milliseconds of int ]
        (** A delay will occur before an event triggers a request. If the event
            is seen again it will reset the delay *)
      | `Throttle of [ `Seconds of int | `Milliseconds of int ]
        (** a throttle will occur after an event triggers a request. If the event
            is seen again before the delay completes, it is ignored, the element
            will trigger at the end of the delay *)
      | `Target of Css.Selector.t
        (** Allows you to filter via a CSS selector on the target of the event *)
      | `From of
        [ `Document
        | `Closest of Css.Selector.t
        | `Find of Css.Selector.t
        | `Window
        ]
        (** Allows the event that triggers a request to come from another element
            in the document (e.g. listening to a key event on the body, to support hot keys) *)
      | `Consume
        (** If this option is included the event will not trigger any other htmx
            requests on parents (or on elements listening on parents) *)
      | `Queue of [ `First | `Last | `All | `None ]
        (** Determines how events are queued if an event occurs while a request
            for another event is in flight *)
      ]

    type non_standard_modifier =
      [ `Load
        (** Trigged on load (useful for lazy-loading) *)
      | `Revealed
        (** Triggered when an element is scrolled into the viewport (useful for lazy-loading).
            If you are using `overflow` you should `intersect once` instead of Revealed *)
      | `Intersect of
        [ `Default | `Root of Css.Selector.t | `Threshold of float ]
        (** Fires once when an element first intersects the viewport *)
      ]

    type t =
      [ standard_modifier
      | non_standard_modifier
      ]

    val to_string : t -> string
  end

  module Poll : sig
    (* TODO: Should this be a more complex type? The docs are pretty vague *)
    type t =
      { condition : string option
      ; interval : [ `Seconds of int | `Milliseconds of int ]
      }

    val to_string : t -> string
  end

  type event_trigger =
    { event : Event.t
    ; modifier : Modifier.t option
    }

  type t = { trigger : [ `Event of event_trigger | `Poll of Poll.t ] }

  val to_string : t -> string
end

module Attributes : sig
  (* Core attributes *)
  val boost : bool -> 'a attrib
  val css : [< Css.t ] -> [> `Class ] attrib
  val delete : Uri.t -> 'a attrib
  val get : Uri.t -> 'a attrib
  val on : string -> 'a attrib
  val patch : Uri.t -> 'a attrib
  val post : Uri.t -> 'a attrib
  val push_url : [ `Bool of bool | `Uri of Uri.t ] -> 'a attrib
  val put : Uri.t -> 'a attrib
  val select : Css.Selector.t -> 'a attrib
  val select_oob : ?swap_strategy:Swap.strategy -> Css.Selector.t -> 'a attrib
  val swap : Swap.t -> 'a attrib
  val target : Target.t -> 'a attrib
  val trigger : Trigger.t -> 'a attrib
  val vals : string * string list -> 'a attrib
  val verb : [< Verb.t ] -> 'a attrib

  (* Extension attributes *)
  val include' : Css.Selector.t -> 'a attrib
end
