defmodule Someapp.Mixfile do
  use Mix.Project

  @source_url "some-url"
  @version "1.6.4"

  def project do
    [
      app: :someapp,
      version: @version,
      elixir: "~> 1.10",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
    ]
  end

  defp package do
    [
      description: "Some description",
      files: [],
      maintainers: [],
      licenses: [],
    ]
  end

  def application do
    [extra_applications: [:crypto, :logger]]
  end

  defp deps do
    [
      {:pkg_a, "~> 0.4.0"},
      {:pkg_b, "~> 1.0.0", optional: true},
      {:pkg_c, "~> 0.22", only: [:dev]},
      {:pkg_d, ">= 1.0.0", only: :test},
      {:pkg_e, "~> 1.1.0", only: [:dev, :test], runtime: false},
      {:pkg_f, git: "https://github.com/f/pkg.git", tag: "1.1.1"},
      {:pkg_g, git: "https://github.com/g/pkg.git", ref: "005dc"},
      {:pkg_h, git: "https://github.com/h/pkg.git", branch: "develop"},
      {:pkg_i, path: "./../some-path"},
      {:pkg_j, ">= 1.0.0", optional: true},
      {:pkg_l, ">= 1.0.0"},
      {:pkg_k, ">= 1.0.0", compile: :rebar},
      {:pkg_m, ">= 1.0.0", manager: [:make]},
      {:pkg_n, ">= 1.0.0", runtime: false},
      {:pkg_o, git: "https://github.com/o/pkg.git", @unknownVariable},
      {:pkg_p, github: "p/pkg"},
    ]
  end

  defp extras do
    []
  end

  defp groups_for_extras do
    []
  end
