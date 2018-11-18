# Simple Blox
Simple and blazing fast Bootstrap 4 Theme for Your blog.

Build using [Bootstrap 4.1.3](https://getbootstrap.com/) dedicated for [Hugo Themes](https://themes.gohugo.io/)

![Hugo Simple Blox Theme](https://raw.githubusercontent.com/okabrionz/simpleblox/master/images/screenshot.jpg)


## Installation

Inside the folder of your Hugo site run:

    cd themes
    git clone https://github.com/okabrionz/simpleblox

For more information read the official [setup guide](//gohugo.io/overview/installing/) of Hugo.


## Getting started

After installing the **Simple Blox Theme** successfully it requires a just a few more steps to get your site finally running.


### The config file

Take a look inside the [`exampleSite`](//github.com/okabrionz/simpleblox/tree/master/exampleSite) folder of this theme. You'll find a file called [`config.toml`](//github.com/okabrionz/simpleblox/blob/master/exampleSite/config.toml). To use it, copy the [`config.toml`](//github.com/okabrionz/simpleblox/blob/master/exampleSite/config.toml) in the root folder of your Hugo site. Feel free to customize this theme as you like.

### Config Settings

```
baseurl = "/"
title = "Your Website Title"
author = "John Doe"
copyright = "Copyright © 2008–2018, John Doe all rights reserved." #You can also use HTML code here.
canonifyurls = true
paginate = 3
theme = "simpleblox" #This parameter is required to use this theme

[params]
facebook = "antihacx"
twitter = "antihacx"
github = "okabrionz"

[menu]
    [menu.main]
    name = "Home"
    url = "/"
    weight = "-10"
```
If You have any feature requests and ideas, please feel free to [submit new Issue](https://github.com/okabrionz/simpleblox/issues/new).

Ready to rock?!

[![Deploy to Netlify](https://www.netlify.com/img/deploy/button.svg)](https://app.netlify.com/start/deploy?repository=https://github.com/okabrionz/simplebloxDeploy)
