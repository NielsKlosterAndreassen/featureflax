defmodule FeatureFlax.PageController do
  use FeatureFlax.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
