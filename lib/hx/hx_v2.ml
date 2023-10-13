[@@@ocaml.warning "-32"]

open Base
open Tyxml_html

module Verb = struct
  type t =
    [ `Delete of Uri.t
    | `Get of Uri.t
    | `Patch of Uri.t
    | `Post of Uri.t
    | `Put of Uri.t
    ]

  let delete_attr uri = Uri.to_string uri |> Unsafe.string_attrib "hx-delete"
  let get_attr uri = uri |> Uri.to_string |> Unsafe.string_attrib "hx-get"
  let patch_attr uri = uri |> Uri.to_string |> Unsafe.string_attrib "hx-patch"
  let post_attr uri = uri |> Uri.to_string |> Unsafe.string_attrib "hx-post"
  let put_attr uri = uri |> Uri.to_string |> Unsafe.string_attrib "hx-put"

  let to_attr = function
    | `Delete uri -> delete_attr uri
    | `Get uri -> get_attr uri
    | `Patch uri -> patch_attr uri
    | `Post uri -> post_attr uri
    | `Put uri -> put_attr uri
  ;;

  let uri = function
    | `Delete uri | `Get uri | `Patch uri | `Post uri | `Put uri -> uri
  ;;

  let uri_to_string verb = verb |> uri |> Uri.to_string

  let verb_to_string = function
    | `Delete _ -> "DELETE"
    | `Get _ -> "GET"
    | `Patch _ -> "PATCH"
    | `Post _ -> "POST"
    | `Put _ -> "PUT"
  ;;

  let to_string verb =
    Fmt.str "%s %s" (verb_to_string verb) (uri_to_string verb)
  ;;
end

module Css = struct
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

  let to_string = function
    | `Added -> "hx-added"
    | `Indicator -> "hx-indicator"
    | `Request -> "hx-request"
    | `Settling -> "hx-settling"
    | `Swapping -> "hx-swapping"
  ;;

  let to_attr css = a_class [ to_string css ]

  module Selector = struct
    type t = string

    let of_string str = str
    let to_string selector = selector
  end
end

module Swap = struct
  module Modifier = struct
    type scroll_behaviour =
      { direction : [ `Top | `Bottom ]
      ; selector : Css.Selector.t option
      }

    let make_scroll_behaviour ?selector direction = { direction; selector }

    let scroll_behaviour_to_string { direction; selector } =
      let direction =
        match direction with
        | `Top -> "top"
        | `Bottom -> "bottom"
      in
      match selector with
      | None -> direction
      | Some selector ->
        Fmt.str "%s:%s" direction (Css.Selector.to_string selector)
    ;;

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

    let to_string = function
      | `Transition transition -> Fmt.str "transition:%b" transition
      | `Swap (`Seconds seconds) -> Fmt.str "swap:%ds" seconds
      | `Swap (`Milliseconds milliseconds) -> Fmt.str "swap:%dms" milliseconds
      | `Settle (`Seconds seconds) -> Fmt.str "settle:%ds" seconds
      | `Settle (`Milliseconds milliseconds) ->
        Fmt.str "settle:%dms" milliseconds
      | `Scroll scroll_behaviour ->
        Fmt.str "scroll:%s" (scroll_behaviour_to_string scroll_behaviour)
      | `Show scroll_behaviour ->
        Fmt.str "show:%s" (scroll_behaviour_to_string scroll_behaviour)
      | `Focus_scrolls focus_scroll -> Fmt.str "focus-scroll:%b" focus_scroll
    ;;
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

  let strategy_to_string = function
    | `InnerHTML -> "innerHTML"
    | `OuterHTML -> "outerHTML"
    | `BeforeBegin -> "beforebegin"
    | `AfterBegin -> "afterbegin"
    | `BeforeEnd -> "beforeend"
    | `AfterEnd -> "afterend"
    | `Delete -> "delete"
    | `None -> "none"
  ;;

  type t =
    { strategy : strategy
    ; modifier : Modifier.t option
    }

  let to_attr { strategy; modifier } =
    match modifier with
    | None -> Unsafe.string_attrib "hx-swap" (strategy_to_string strategy)
    | Some modifier ->
      Unsafe.string_attrib
        "hx-swap"
        (Fmt.str
           "%s;%s"
           (strategy_to_string strategy)
           (Modifier.to_string modifier))
  ;;
end

module Target = struct
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

  let to_string = function
    | `This -> "this"
    | `Closest selector ->
      Fmt.str "closest:%s" (Css.Selector.to_string selector)
    | `Css_selector selector ->
      Fmt.str "css-selector:%s" (Css.Selector.to_string selector)
    | `Find selector -> Fmt.str "find:%s" (Css.Selector.to_string selector)
    | `Previous selector ->
      Fmt.str "previous:%s" (Css.Selector.to_string selector)
  ;;

  let to_attr target = Unsafe.string_attrib "hx-target" (to_string target)
end

module Trigger = struct
  module Event = struct
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

    let to_string = function
      | `Focus -> "focus"
      | `Focus_in -> "focusin"
      | `Focus_out -> "focusout"
      | `Blur -> "blur"
      | `Key_down -> "keydown"
      | `Key_press -> "keypress"
      | `Key_up -> "keyup"
      | `Mouse_enter -> "mouseenter"
      | `Mouse_leave -> "mouseleave"
      | `Mouse_move -> "mousemove"
      | `Mouse_out -> "mouseout"
      | `Mouse_over -> "mouseover"
      | `Mouse_up -> "mouseup"
      | `Mouse_wheel -> "mousewheel"
      | `Change -> "change"
      | `Click -> "click"
      | `Submit -> "submit"
      | `Custom str -> str
      | #t -> .
    ;;
  end

  module Modifier = struct
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

    let to_string = function
      | `Once -> "once"
      | `Changed -> "changed"
      | `Delay (`Seconds seconds) -> Fmt.str "delay:%ds" seconds
      | `Delay (`Milliseconds milliseconds) -> Fmt.str "delay:%dms" milliseconds
      | `Throttle (`Seconds seconds) -> Fmt.str "throttle:%ds" seconds
      | `Throttle (`Milliseconds milliseconds) ->
        Fmt.str "throttle:%dms" milliseconds
      | `Target selector ->
        Fmt.str "target:%s" (Css.Selector.to_string selector)
      | `From `Document -> "from:document"
      | `From (`Closest selector) ->
        Fmt.str "from:closest:%s" (Css.Selector.to_string selector)
      | `From (`Find selector) ->
        Fmt.str "from:find:%s" (Css.Selector.to_string selector)
      | `From `Window -> "from:window"
      | `Consume -> "consume"
      | `Queue `First -> "queue:first"
      | `Queue `Last -> "queue:last"
      | `Queue `All -> "queue:all"
      | `Queue `None -> "queue:none"
      | `Load -> "load"
      | `Revealed -> "revealed"
      | `Intersect `Default -> "intersect"
      | `Intersect (`Root selector) ->
        Fmt.str "intersect:root:%s" (Css.Selector.to_string selector)
      | `Intersect (`Threshold threshold) ->
        Fmt.str "intersect:threshold:%f" threshold
      | #t -> .
    ;;
  end

  module Poll = struct
    (* TODO: Should condition be a more complex type? The docs are pretty vague *)
    type t =
      { condition : string option
      ; interval : [ `Seconds of int | `Milliseconds of int ]
      }

    let to_string { condition; interval } =
      let interval_str =
        interval
        |> function
        | `Seconds s -> Fmt.str "%ds" s
        | `Milliseconds ms -> Fmt.str "%dms" ms
      in
      match condition with
      | None -> Fmt.str "every %s" interval_str
      | Some condition -> Fmt.str "every %s %s" interval_str condition
    ;;
  end

  type event_trigger =
    { event : Event.t
    ; modifier : Modifier.t option
    }

  type t = { trigger : [ `Event of event_trigger | `Poll of Poll.t ] }

  let to_string { trigger } =
    match trigger with
    | `Event { event; modifier } ->
      let modifier_str = modifier |> Option.map ~f:Modifier.to_string in
      (match modifier_str with
       | None -> Event.to_string event
       | Some modifier_str ->
         Fmt.str "%s;%s" (Event.to_string event) modifier_str)
    | `Poll poll -> Poll.to_string poll
  ;;

  let to_attr trigger =
    trigger |> to_string |> Unsafe.string_attrib "hx-trigger"
  ;;
end

module Attributes = struct
  (* Core attributes *)
  let boost enabled =
    enabled |> Bool.to_string |> Unsafe.string_attrib "hx-boost"
  ;;

  let css c = Css.to_attr c
  let delete = Verb.delete_attr
  let get = Verb.get_attr
  let on event = Unsafe.string_attrib "hx-on" event
  let patch = Verb.patch_attr
  let post = Verb.post_attr

  let push_url = function
    | `Bool b -> b |> Bool.to_string |> Unsafe.string_attrib "hx-push-url"
    | `Uri uri -> uri |> Uri.to_string |> Unsafe.string_attrib "hx-push-url"
  ;;

  let put = Verb.put_attr

  let select selector =
    selector |> Css.Selector.to_string |> Unsafe.string_attrib "hx-select"
  ;;

  let select_oob ?swap_strategy selector =
    match swap_strategy with
    | None ->
      selector |> Css.Selector.to_string |> Unsafe.string_attrib "hx-boost-oob"
    | Some swap_strategy ->
      Unsafe.string_attrib
        "hx-boost-oob"
        (Fmt.str "%s:%s" selector (Swap.strategy_to_string swap_strategy))
  ;;

  let swap = Swap.to_attr
  let target = Target.to_attr
  let trigger = Trigger.to_attr

  (* Extension attributes *)
  let vals (name, values) =
    let values = String.concat ~sep:"," values in
    Unsafe.string_attrib name values
  ;;

  let verb = Verb.to_attr

  (* Extension attributes *)
  let include' selector =
    selector |> Css.Selector.to_string |> Unsafe.string_attrib "hx-include"
  ;;
end
