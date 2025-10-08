function setFrames( win, top, left, body )
{
  if( left != null && left != "" )
  {
    win.left.location = left;
  }

  if( top != null && top != "" )
  {
    win.top_frame.location = top;
  }

  if( body != null && body != "" )
  {
    win.body.location = body;
  }

	win.focus();
}
