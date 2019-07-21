# JPModLib

**ModLib**

- [Overview](#overview)
- [Components](#components)
  1. [TJPEsGradient](#tjpesgradient)
  1. [TJPPegTopTrackBar, TJPPegTopColorTrackBar](#tjppegtoptrackbar-tjppegtopcolortrackbar)


---

## Overview

**JPModLib** - Delphi components.

Modified components from other open source packages / libraries.

---

## Components


### TJPEsGradient

A modified version of the `TEsGradient` from the **TurboPack Essentials** package (https://github.com/TurboPack/Essentials).

<p align="center">
<img src="./docs/img/TJPEsGradient.png">
</p>

My modifications:
* `gGradColors` array moved to the **public** part
* `DrawBorder`
* `BorderColor`
* `TagExt`

In one of my applications I needed a gradient component with access to all partial colors. **TEsGradient** turned out to be almost perfect. All generated colors are saved in the `gGradColors` array. In the original unit, it is declared in the **private** part, so it was necessary to move it to the **public** section.

By the way, I added the option of drawing borders.

Original license: [MPL 1.1](https://www.mozilla.org/en-US/MPL/1.1/)  
License for my modifications: **You can do with my code whatever you want without any cost and without any limitations.**

---

### TJPPegTopTrackBar, TJPPegTopColorTrackBar

An advanced track bars for Delphi.

A modifed `TPegTopTrackBar` and `TPegTopColorTrackBar` from the **PegTop Common Components** written by Jens Gruschel
(http://www.pegtop.net/delphi/components/common/index.htm).

<p align="center">
<img src="./docs/img/PegTopTrackBars.png">
</p>

My modifications:
* A large amount of code related mainly to the appearance of the control.
* `PositionLabel` - External label displaying the current position. Track bars also have internal labels, but they can only be set above and below the control.
* Ticks support
* `TagExt`

[PegTop license](./libs/Pegtop/PegTop_License.txt)  
License for my modifications: **You can do with my code whatever you want without any cost and without any limitations.**


