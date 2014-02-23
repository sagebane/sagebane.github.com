---
layout: default
title: Man of the end of the world
---
<ul>
{% for post in site.posts %}
  <li>
   <span class="date">{{ post.date | date_to_string }}</span> <a href="{{ post.url }}">{{ post.title }}</a>
    <div class="clear"></div>
  </li>
{% endfor %}
</ul>
