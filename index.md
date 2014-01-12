---
layout: default
title: Non-geek's page
---
# Who is this?

I'm a programmer, but not a good one. 

Wanna be a geek, but not a geek honestly.

It seems still take me serveral years to find WHO I am, and I will try it hard.

# Related posts

<ul class="posts">
{% for post in site.posts limit: 5 %}
  <div class="post_info">
    <li>
            <a href="{{ post.url }}">{{ post.title }}</a>
            <span>({{ post.date | date:"%Y-%m-%d" }})</span>
    </li>
    <em>{{ post.excerpt }} </em>
    </div>
  {% endfor %}
</ul>
