---
layout: default
title: sgbn's blog posts
---

<ul>
{% for post in site.posts %}
	{% if post.visible== 1  %}
	<li>
	<span class="date">{{ post.date | date_to_string }}</span> <a href="{{ post.url }}">{{ post.title }}</a>
    <div class="clear"></div>
	</li>
  	{% endif %}
{% endfor %}
</ul>
