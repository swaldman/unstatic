# Unstatic

_Unstatic_ means to be a static-site-generator generator
(really a generator of generators of sites that can be
static, semi-static, or dynamic).

## Getting Started

### 1. Set-up an _untemplate_ project

An _unstatic_ project begins as an _untemplate_ project.
Let's [get that started](https://github.com/swaldman/untemplate-doc#quickstart)!

```zsh
% sbt new swaldman/untemplate-seed.g8
```

Edit the resulting `build.sc` file to include `unstatic` dependencies.

`unstatic` is divided into a library of base utilities, and the current implementation
in terms of [zio](zio.dev) and [tapir](https://tapir.softwaremill.com/).
So you'll want two dependencies.

I like to define my dependencies in [mill] projects as constants in a top-level
object called `Dependency`. _(You can organize this stuff however you like!)_
I place something like this above the module definition in `build.sc`:

```scala
val UnstaticVersion = "0.0.4"

object Dependency {
  val Unstatic             = ivy"com.mchange::unstatic:${UnstaticVersion}"
  val UnstaticZTapir       = ivy"com.mchange::unstatic-ztapir:${UnstaticVersion}"
}
```

Then, inside the module definition, I override mill's `ivyDeps` task:

```scala
  override def ivyDeps = T {
    super.ivyDeps() ++
      Agg (
        Dependency.Unstatic,
        Dependency.UnstaticZTapir,
      )
  }
```






