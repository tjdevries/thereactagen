open Tyxml

let links_box =
  [%html
    {|
  <ul class=links_bar id=links_bar>
    <li id=home_click >My Musings</li>
    <li id=about_click >About Me</li>
    <li id=blog_posts_click >Blog</li>
    <li id=hackathons_click >Hackathons</li>
  </ul>
|}]
;;
