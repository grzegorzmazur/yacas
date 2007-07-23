

/* When deep link found, redirect to the tabbified main page */
if (top.location == self.location)
{
  self.location.replace("../homepage.html?"+self.location.href);
}

