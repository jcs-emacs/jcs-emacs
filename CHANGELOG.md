# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


## 9.2.1 (Unreleased)
> Released N/A

* feat(lisp): Add `nerd-icons` for `xref` ([`ae67ce4`](../../commit/ae67ce48a7466ab676f1bff5f6f048482e04b395))
* fix(module): Load diff config for `diff-mode` ([`2372ebf`](../../commit/2372ebf5f69baf445cfcc1024bd9497dfe09c5f7))
* feat(vc): Add new package `diffview` ([`2155ec9`](../../commit/2155ec9bda2956662b9a3699d50044664ad5ad38))
* feat(emacs): `auto-revert` is require by `dired` ([`77d26ca`](../../commit/77d26cab3e7eae19cd62dd771890b501407329a0))
* feat(emacs): Add new package `dired-git-info` ([`f476865`](../../commit/f4768651008445f1cf72db7adc1a81d4afa975d7))
* feat(emacs): Add new package `dired-gitignore` ([`77d26ca`](../../commit/77d26cab3e7eae19cd62dd771890b501407329a0))
* feat(emacs): Add new package `dired-efap` ([`95961e1`](../../commit/95961e120635f66ad2ea8c18033c7b420743007b))
* fix(emacs): `dired-efap` doesn't compatible to `dired-git-info` ([`cce8775`](../../commit/cce877589ae9bdc13881a64cc6e896caefef5911))
* feat: Add `jest-test-mode` ([`ad7b67f`](../../commit/ad7b67f3a8860e2b0f4fa232b18918a05fbfcf06))
* fix(ui): Add tab for `comint` and `compilation-mode` ([`30e19ba`](../../commit/30e19bab52f59c00dc74624c96bf48e193c5d6a8))
* feat(lisp): Add `python-pytest` ([`9b480e6`](../../commit/9b480e6e5713d53c70ce14c3958ce2334931954b))
* feat(lisp): Add `company-graphql` ([`d58ea91`](../../commit/d58ea91c4e1ebaf03669cbedea79848caabbf402))
* feat(lang): Add module for GraphQL ([`b2cf8d3`](../../commit/b2cf8d38106e18c33a8501e4b5962b6ef69da30b))
* feat(lisp): Add `psc-ide` for PureScript for IDE-like experience ([`f72f431`](../../commit/f72f4314429bda9bda7d4772aaf580c688d839ad))
* feat(lisp): Add `psci` for PureScript ([`7c25aa5`](../../commit/7c25aa56a3ecb97270d1199f3dfa6bf536910029))
* feat(lisp): Add `graphviz-dot-mode` for [Graphviz][] ([`452f0b0`](../../commit/452f0b08d07561f02a7d8ae14b7a76687d369e9c))
* feat(lang): Add module for [Graphviz][] language ([`ecd70ca`](../../commit/ecd70ca9dbf5c8012897695ef7da74667e94346a))
* feat(lang): Add basic EJSON support ([`7d41e97`](../../commit/7d41e97eac3f9c70136d48b3536c3c711588864b))
* feat(lisp): Add basic Faust support

## 9.2.0
> Released Jul 21, 2025

* feat: Replace must kill buffer list with derived mode ([`c407b98`](../../commit/c407b9859489c5b31697c9f61871b9e8254baaf7))
* feat: Add new package `responsive-window` ([`0af7dfc`](../../commit/0af7dfcc565b3813ac8776c6f75bf15fc5c74ea5))
* fix: Record the window starting point instead first visible line ([`62039e7`](../../commit/62039e725e816ad381b6a8d126bb28be22d65101))
* fix: Force load project elisp files ([`b663d26`](../../commit/b663d26669d35c69d0c99dc3a8353946fd2cb7cf))
* fix: Load theme before `on` prevent potential UI package's errors ([`0c995aa`](../../commit/0c995aa207eb820d7b0d0778aa23da2c96b12e5f))
* feat: Add `Coq` support ([`c8382d9`](../../commit/c8382d9103f98bc4d1015c48bddca0b004018a92))
* feat: Add `GDShader` support ([`fdb485a`](../../commit/fdb485a4adb0d29284ca93de9647e47b1bf7f94b))
* feat: Add `Janet` support ([`2738bdc`](../../commit/2738bdc83f2b2d00411e9f5bb489087d1ae27c42))
* feat(lang): Add `geiser` for better `Scheme` support ([`b981c72`](../../commit/b981c72a0942ca13a4f9fc0da7edcfd4653482fd))
* feat: Configure `eldoc-diffstats` ([`cb2afb0`](../../commit/cb2afb004e92d88b54db1506da54d2f43bc9a87f))
* feat: Add `magit` entry functions ([`8f96865`](../../commit/8f9686563559f7b209c289fe24e5d2a6ea9f7c15))
* fix: Annoying `when-let` deprecation ([`1e4ca28`](../../commit/1e4ca284a4e28d9f333fb3b6337dfdcbb1008a60))
* fix: Annoying `if-let` deprecation ([`4a002df`](../../commit/4a002dfb13987d967123e1a9c5ba8ed9c80d2fce))
* feat(dired): Make list directories first in `dired` ([`a502ee4`](../../commit/a502ee445dcfa9d83eff262f9381897044e9c9fd))
* feat(ui) Enable `sideline` by default ([`ebf0537`](../../commit/ebf0537358f3de9f8204f4421b4fef96e487c6d5))
* feat(vc): Add more `magit` plugins ([`e47a8d0`](../../commit/e47a8d0e873001d769eb39c3e85de9fb0a591897))
* perf(lsp): Add `lsp-smart-req` ([`fdcddb5`](../../commit/fdcddb59e7558346d3dfe80639e8d91f4d7f5890))
* feat(vc): Use `magit` built-in functionalities to display file icons ([`7d249ad`](../../commit/7d249ad694a58239d3247b673f8e15a92adc4a18))
* feat(ui): Add mail tab group ([`e0ec5c2`](../../commit/e0ec5c24c7902f862d4793ffdf8fece6bb14076b))
* feat(fold): Add `savefold` for persistent folding behaviors ([`98d0f49`](../../commit/98d0f498ccb2d16326f8814c7d4d36a4e2279d51))
* feat(debugger): Combine `debugger` and `run` functionality ([`2d40485`](../../commit/2d4048562fc019da0acc6bbc55242d1767cfc90d))
* feat(fold): Add new package `fold-this` ([`5c9b423`](../../commit/5c9b423ba95419dcb8e81477570778739c1ec851))
* feat(keys): Add support for `defaul-text-scale` ([`4453b90`](../../commit/4453b907e5702989ca711c90b3244e04b8f873c6))
* feat(lang): Add support for `Magik` language ([`5ab68e5`](../../commit/5ab68e5a9deb1671a55c36ca81ea866217f2cfd2))
* feat(lang): Add `Odin` support ([`a69d5c1`](../../commit/a69d5c11fb3707c92c47bbe29470e7a3eae8bce3))
* feat(core): Add `back-button` ([`4da8f3d`](../../commit/4da8f3dbcb3ade1598643e2de1ffae5c42b00a4f))
* feat(lang): Add `elvish` mode ([`9a4b6e3`](../../commit/9a4b6e399e2a7f856f4dcb5cacfcc56097e4d32b))
* feat(tools): Add `dirnenv` config ([`2346542`](../../commit/2346542012b66789f0c6fcad65ce9332ff90d7af))
* feat: Use `kill-current-buffer` command instead ([`0b7e91a`](../../commit/0b7e91a56c34c5c3b4c74aa04dae029154899cb2))
* feat(lang): Config more `eldoc` for elisp ([`9db9cb0`](../../commit/9db9cb01ded4a01bb483a78b51a6b35f4ee21e2e))
* feat(emacs): Add `nerd-icons` for `grep` ([`e6fedfe`](../../commit/e6fedfeadc0d68ca033f9480c10791da2260b337))

## 9.1.0
> Released Jun 19, 2024

* Add FASM support ([`e049b01`](../../commit/e049b015df8a52d10271e3a41aeeca631296a7d5))
* Remove package `topsy` ([`77c5067`](../../commit/77c506758eaca4621cf1a92e9107e5d279721827))
* fix: Apply workaround for `highlight-indent-guides` ([`e153bd8`](../../commit/e153bd8fd60ec3f6ed11cd8494e94feb7d1cb7c3))
* feat: Add Tramp support ([`8535af3`](../../commit/8535af30d96e173df46515f03e91d50e472dca5e))
* Add PO file support ([`59fa6ba`](../../commit/59fa6ba7d8f30be3f16fe849f41fd3c21a44a852))
* Remove built-in log module, use `ic` instead ([`70ee1d0`](../../commit/70ee1d002ff97cb765b3043e7451f2aeb831e51c))
* Extract template module to external package ([`74a2cb8`](../../commit/74a2cb84a0fd322d6b4cb73c7a1c0c68cb135f42))
* Add `.envrc` support ([`3ff057f`](../../commit/3ff057fc9cfa0c931129e6f5496a2eae4211a34e))
* Update debugging key bindings ([`f2ecb5d`](../../commit/f2ecb5dcb94d8abd373d8a3e3c39049029a65a62))
* Add QSS support ([`71a175c`](../../commit/71a175c4adcfd913131030fb1deb5d2e5f9cc4f0))
* feat: Add and configure Tex completion packages ([`31deaa9`](../../commit/31deaa9a9850ae904b9325af2b39dc5a57d0d82b))
* refactor: Move message clean settings to its modules ([`727bc53`](../../commit/727bc53d323580215b7ad3a5cb8b5b84c1b14e3f))
* feat: Support TailwindCSS completion ([`a3b065d`](../../commit/a3b065dba11f27566fce2e24492157138d775646))
* feat: Support Bootstrap completion ([`a3b065d`](../../commit/a3b065dba11f27566fce2e24492157138d775646))
* feat: Add Svelte support ([`e586d9e`](../../commit/e586d9e52d06a054faa1829ba0cf69c128481c34))
* feat: Add ziglint ([`9856bbf`](../../commit/9856bbf5530177613e46127b67054d3925f99a91))
* feat: Add module util ([`12f7d29`](../../commit/12f7d294e66df3eddc6bbec721ef4196144dcf8f))
* feat: Support `daemon` ([`cdb7187`](../../commit/cdb7187af574c6aec3ff09010d59fe2a8814f43b))
* chore(sideline): Add display mode config ([`3232499`](../../commit/32324992b47b2126241a9ab84410ba7cfdac3752))
* chore(dashboard): Default to cycle dashboard sections ([`1956896`](../../commit/1956896dd3f85e1fafb980c8aff5dbf01b1a2507))
* fix(buffer-menu): Ensure `buffer-menu` is clean ([`548ce7e`](../../commit/548ce7ef47490fa8d0b82b838cc4f8a1e739494b))
* chore(sideline): Configure `sideline-eglot` ([`def9065`](../../commit/def906559bf90920ff7f2f2df4b8f36dd523230a))
* chore(checker): Mute display diagnostic ([`675b982`](../../commit/675b982533c978624ff5e745d1621edcb7a0d994))
* chore: Add `guard-lf` package ([`d861622`](../../commit/d8616223fb07503dffb3ec4ca75361b203ab5b76))
* feat: Notify the user when `indent-tabs-mode` got activated ([`7b4eb11`](../../commit/7b4eb11cf30fae0309445fa86ac8749a1f276ed8))
* feat: feat: Replace package `doxygen-asterisk` with `auto-close-block` ([`2d56f44`](../../commit/2d56f447ac48f89654fbd93872a32b83f8e40b3c))
* fix(vc-gutter): Ensure `diff-hl` is updated in all valid buffers ([`5380220`](../../commit/53802208c3251cf0f7bd2a0043f34c133a21ee1a))
* feat(rgb): Replace `rainbow-mode` with `colorful-mode` ([`695358c`](../../commit/695358cbb6475c54aba711615d6f7db61fc9c3fc))
* feat(tools): Configure `sqlite-mode` ([`73dc3b0`](../../commit/73dc3b0adea778ba5c933cb0c203ec3e82bd6979))

## 9.0.1
> Released Sep 20, 2023

* Add new package `makefile-executor` ([`e301333`](../../commit/e301333ef7585189614bb1fed163caab2e4a0973))
* Add new package `codegpt` ([`443b2e1`](../../commit/443b2e1312368201a8a15d64355e7c9aaf5b8958))
* Add and config new package `goto-last-change` ([`71d0c3d`](../../commit/71d0c3dfa0636fc55cc1c023e6383d5946b19e3c))
* Add new package `flycheck-clang-analyzer` ([`0ea36fe`](../../commit/0ea36feb652c0ebe59ab5b80d5739cb23cb4b52b))
* Add new package `codemetrics` ([`51032ef`](../../commit/51032ef78f5b9827ee76c5c48832974c9bd9cf73))
* Bind <kbd>Shift</kbd>+<kbd>return</kbd> to insert `newline` in minibuffer ([`41c159b`](../../commit/41c159ba7d5630fb8e7e235fc8968001f04cbb15))
* Ask CMake file-header template to differentiate `default`, `root`, and `subdirectory` ([`71c9bc6`](../../commit/71c9bc604b57cb2379606a0b6bc12c42bb79ded7))
* Update auth-source logic for more general use ([`8681fb6`](../../commit/8681fb6800d4b4090c7a84dfd9d927035e2bb66a))
* Add `company-dict` ([`1b24a4d`](../../commit/1b24a4ddd58943fb37010b7700dac32c8d54bea1))
* Add nerd-icons support ([`3ae70b9`](../../commit/3ae70b96937dedae3b1fe8fc9edf52ad340795d6))
* Enable comment empty lines by default ([`2040d77`](../../commit/2040d77e956cc3c1fb55ec0dae7dfe6c36c2381b))
* Fix cannot re-connect to language server issue ([`362a964`](../../commit/362a96418032da4244c05c810031ed74bc7b25c4))
* Add dashboard's navigator ([`d81fce7`](../../commit/d81fce7fb699f1866839c0bd409475ab758721d4))
* Fix `indent-guides` is not obvious in 29.x ([`8456bd0`](../../commit/8456bd051b096af816cd9c0840b6980c21d94c5e))
* Add `sly` to support common lisp development ([`fa0afd2`](../../commit/fa0afd2d79b8b464f4f2413fcd93978ef168402f))
* Add support for OpenCL ([`d5723c3`](../../commit/d5723c34ac53c7983cdfb686884478bcacc4d4a6))
* Add support for CUDA ([`d5723c3`](../../commit/d5723c34ac53c7983cdfb686884478bcacc4d4a6))
* Use `context-menu-mode` instead of `right-click-context` ([`a7e563e`](../../commit/a7e563ea23121dd6ba340eb8fd8b9daed394f019))
* Add more support for Clojure language ([`b84287b`](../../commit/b84287bde0315c021f73009fcd67e2a0d348d64f))
* Add breadcrumb mode ([`a81fcc6`](../../commit/a81fcc65af7357eeb4babb58b97b567c5a453b01))
* Use prog mode settings for `haskell-cabal-mode` ([`fd8dfb5`](../../commit/fd8dfb5108882e5fc5a1ad74b129e35c3e1ae762))
* Add support for P4 ([`3b16789`](../../commit/3b16789978d380b6be75f465fa2a3d3dbd9610b1))
* Add company support for PHP ([`30c8b00`](../../commit/30c8b00169eefb866df062095c1bf33b6d0729ec))
* Add company support for Perl ([`98d4e14`](../../commit/98d4e1494364ca49bf3cd370a659957674eda100))
* Configure for beancount ([`40ee14e`](../../commit/40ee14e541453e684d1f836dcc2fa47abda9ece9))
* Add support for F* ([`1dc616a`](../../commit/1dc616adae9bdcbf1810058b3d078f831be5cd34))
* Add support for Hylang ([`df2337b`](../../commit/df2337bfb3ef64a3c2e9cbd5422df7eb66b9e698))
* Add support for Scheme ([`6fe51f4`](../../commit/6fe51f46592cd3e6ceee1ae274b086b046dc93b0))
* Add support for ledger ([`5cc7ba8`](../../commit/5cc7ba8abad0864f9d435519bb35bf45b32d2dcb))
* Add support for SML ([`031289b`](../../commit/031289bf682aa956cd322e1e1078408f0d79e8ce))
* Use built-in on first project hook ([`84a7695`](../../commit/84a7695b668debed6fb59bbd5313ff9be897d6b7))
* Add LaTex support ([`7fbb631`](../../commit/7fbb631c54b82aa43a80c4ad7733143c89773ff1))
* Add Noir support ([`7fbb631`](../../commit/7fbb631c54b82aa43a80c4ad7733143c89773ff1))
* Enable jump to button in Tree-Sitter debug mode ([`02f5172`](../../commit/02f5172c7d061732bb0f154ded5898c1baaf16b9))
* Add Windows Menu's key bindings ([`0cc2bb8`](../../commit/0cc2bb8d938f6a549cc3ae9a15ba4776010c6fa8))
* Add package `chatgpt-sideline` ([`65409fc`](../../commit/65409fc4c68254b18ce209e64fcf74b016e2413e))

## 9.0.0
> Released Jan 01, 2023

* Add new package `company-kaomoji` ([`1cc6b70`](../../commit/1cc6b70e9ab4ae3fc893482c1d38986da6b32e87))
* Refactor `ts-fold-indicators` face function and get queries function ([`27e6148`](../../commit/27e614884c1cb84d8f832280d253d387cdde99b4))
* Add OCaml support ([`ddaaf11`](../../commit/ddaaf11b98c88c709fecd5db8b8214a63e1bb00f))
* Add support for `elfeed` ([`7a3c3ad`](../../commit/7a3c3ad99bea5088da2c7d681552f55914b37934))
* Cancel `echo-bar` right padding by default ([`75416a7`](../../commit/75416a77c6bb0eef90b71a2ee09ab42202bc7bb5))
* Add new packages `org-superstar` and `org-fancy-priorities` ([`695f5f4`](../../commit/695f5f46e611de1529c6bbd57cd251386616d2f7))
* Add new package `hammy` ([`8ea21aa`](../../commit/8ea21aa67e98efd9a8eb846e6b908fb500afab52))
* Enable `display-fill-column-indicator-mode` in `text-mode` by default ([`5283fd9`](../../commit/5283fd976574ecd322e77556bcd0b6790eed7403))
* Enhance experience with `org-superstars` ([`5c240a6`](../../commit/5c240a626fe60685f2768eb17a6dbfcc61452134))
* Add new package `sideline-color` ([`6c472aa`](../../commit/6c472aa4c92069551a0707760e3d41c12dcd7411))
* Add support for `haml-mode` ([`b50bbb2`](../../commit/b50bbb26cfddab3e16d849f65e33894ec5a70177))
* Use package-menu to upgrade packages instead of the prompt ([`5903e43`](../../commit/5903e431921a10f023247ba59ee78eaaa59fc961))
* Extract message utility functions to separate module, `msgu` ([`d3957be`](../../commit/d3957bec1ffd3668a5f8861644f6ec2953cd3781))
* Fix does upgradable menu after package list is refreshed ([`09d7840`](../../commit/09d78404f7e8f0bec64d4df78ae9f90e7c6069d9))
* Add clear filter key for `package-menu` mode ([`bf36721`](../../commit/bf36721805a48b6319bd5eb1b9064129af293560))
* Load `eask-api` when in valid Eask related project ([`5644b51`](../../commit/5644b5128cb1f132b9b552cc745377be8528b435))
* Add new package `editorconfig-generate` ([`afd3fa0`](../../commit/afd3fa07135fd9154b551fffb20822316cf4e3d7))
* Bind key `C-M-k` to `kill-current-buffer` ([`758c464`](../../commit/758c464dfe76bad0053ce82f4064f382a1ae4ac3))
* Remove `buffer-menu` feature on display project name ([`74feaa9`](../../commit/74feaa938d4be1667bab1c1f68308f8c6a250208))
* Use macro to define `file-header` insertion functions ([`e5e2006`](../../commit/e5e200684f25325c70906a53a84f526328bc6fbf))
* Re-define file header source with utility macro ([`ca4d8e4`](../../commit/ca4d8e49ba80f78bd34ff610a442c8a8bf7bc097))
* Turn `completion-ignore-case` on by default ([`b94542c`](../../commit/b94542cb51736f26a3842753bbc268116fa4ba06))
* Add new package `minimap` ([`ae9ef5a`](../../commit/ae9ef5a0d93bf8f924c8572d00f026dace2079a3))
* Replace package `docstr` with `ts-docstr` for better parsing capability ([`ee85ba4`](../../commit/ee85ba4b01401707461ec695038f2ee4cc175f74))
* Add new package `highlight-doxygen` ([`ee85ba4`](../../commit/ee85ba4b01401707461ec695038f2ee4cc175f74))
* Enable `minibuffer-depth-indicate-mode` by default ([`83ecd6c`](../../commit/83ecd6c2b256d2fa0dd3b6fa64236ad5aab2843f))
* Fix typescript insertion with `ts-docstr` ([`ec8a559`](../../commit/ec8a5592e8f4e6ba707d36218461e2f0a9329822))
* Add new package `toggle-profiler` ([`eecf032`](../../commit/eecf0323d3af8fdcc2b3c2c8e8e62dd78774a3b7))
* Bind new key to command `keyboard-escape-quit` ([`3ce9d3f`](../../commit/3ce9d3ff4ae965a31f1855bf6f9b11c31e2576be))
* Bind new key to command `toggle-profiler` ([`f49dfeb`](../../commit/f49dfeb07c1e0d3d879cb3acdfb4a5ab892326f2))
* Bind new key to command `ts-docstr-ask` ([`9836e3d`](../../commit/9836e3d76afeadcb75c0d7378d38b209dda50e82))
* Add new packages `gitlab-ci-mode` and `gitlab-ci-mode-flycheck` ([`984fc83`](../../commit/984fc836ecb832e047409a1efa03c806205a7c9e))
* Remove unnecessary config to `auto-mode-alist` ([`0dad622`](../../commit/0dad6227b3372d530f55032726693ab41541e8a7))
* Add new package `sln-mode` to handle `*.sln` file ([`673a263`](../../commit/673a2635efccd1e5258a157e738f8ebdb35f567a))
* Add new font `Symbola.ttf` for Windows' unicode display ([`98a7455`](../../commit/98a74550b9edfcbe30d403418263b6594582652c))
* Extract undo/redo module to external package `undo-tree-vf` ([`391bb1e`](../../commit/391bb1e4b1416c1c7f54a19f07c6b83ccd3e4524))
* Use built-in `bolp` and `eolp` functions instead ([`0504aa5`](../../commit/0504aa597efd52e9efbefe3e2338349cc5d458fc))
* Extract parent directory not found when file creation module to external package `ff-guard` ([`0085929`](../../commit/008592968fea55e925c0bbe8e5412b93a4dde7c3))
* Add new package `prettier` ([`a2af4f7`](../../commit/a2af4f74eea9e6b7861db8babb1de2a4909247a2))
* Add new package `vs-electric-spacing` ([`a04369b`](../../commit/a04369b5ebfa1db7a47bcf156f80614959685190))
* Add new package `terminal-here` ([`56ad79f`](../../commit/56ad79fdf537fc1d7380ff4adc71dea5051e00ac))
* Add new package `quickrun` ([`e13899b`](../../commit/e13899b04affe5157a0cacff52b24c1925c293f4))
* Extract select file in project/pwd to external `ffap` ([`e9b7c04`](../../commit/e9b7c045a2cbd2dfdfc4f519e24a973a0807410b))
* Extract [@cmuratori](https://github.com/cmuratori)'s run/make script feature to external `execrun` ([`83d90d5`](../../commit/83d90d53e5324e22e5fbf3c174487ddd22fd7117))
* Extract find corresponding file module to external package `fof` ([`b7d018b`](../../commit/b7d018be296f55ca5cd8859b53c68efe326a2399))
* Extract VSCode editing experience to external package `vsc-edit-mode` ([`744eb52`](../../commit/744eb525d3c90055528f650e3991614eb172684a))
* Removed complex line number configuration, keep it simple ([`f78fd0b`](../../commit/f78fd0b8b96b8fb113f6156d346689622967fa15))
* Remove dashboard's previous/next blank-lines navigation keys ([`bfef0c1`](../../commit/bfef0c196ac7fdc307552b6d5f663279d540cba4))
* Extract package module to external package `pkg-dm` ([`9051c25`](../../commit/9051c25ce5c07542ea0a52bfb4192d74ac8d1453))
* Use `elenv` for emacs-lisp environment ([`92c9db4`](../../commit/92c9db4dbbe49072874fd18d2c27b44a5384bfd4))
* Remove previouse/next key type option ([`a4a605f`](../../commit/a4a605f44e18ae2161a8e87598e636bf9f1f51a9))
* Extract previous/next blank line keys to external package `block-travel` ([`9f624c1`](../../commit/9f624c1bbb7259da65fd9b468812358d53ee36ca))
* Add packages for feature sticky header, `topsy` and `org-sticky-header` ([`a9f01c9`](../../commit/a9f01c9ce319e27e04e1a8fac3b56ed14f3d65ab))
* Add support for Terraform ([`d241fc7`](../../commit/d241fc77d5c13b3e01e1d0dfd78147b3a7c9a59d))
* Add support for Zig ([`2911e87`](../../commit/2911e870dce6238ab6d960a287cd25c64576bb33))
* Add support for Racket ([`f2f314f`](../../commit/f2f314f1eb670b7508c661cf68e0033292fe256c))
* Add support for Idris ([`9901371`](../../commit/99013713d649e59f07fae5a2bf9e7fe5f96f0800))
* Add support for VHDL ([`185046a`](../../commit/185046a34e06bc8c4a6abf57c4f6d843a66da722))
* Add support for Mint ([`9d50d8a`](../../commit/9d50d8a05767f32c373f13240f3eaeb9440f6582))
* Improve EWW UX ([`b954b8f`](../../commit/b954b8f5f1782a500eb4a7fda688a8fab0aebd27))
* Improve `image-mode` UX ([`80127d5`](../../commit/80127d5060bbf6ae871d87502dfff27a73c123fe))
* Add `emp` as default music player ([`2861743`](../../commit/2861743073ecd2e6d7608dac8021fc400104c4bb))
* Bind keys for `emp` ([`f5183be`](../../commit/f5183bee81fea5b0e832ed39282ad76ee9ed70e8))
* Bind keys for `eww` ([`500696b`](../../commit/500696b4403a50f83951532f0e31ab4b1cd5a2bb))
* Add support for fish shell-script ([`44ed976`](../../commit/44ed9764a7f2dc3e3632c6b0136390a97bce74e4))
* Add support for ansible ([`cb91bee`](../../commit/cb91beeff65b63539a2893f67aaa888d9d53dc7b))
* Remove `rjsx-mode` and `js2-mode`, use default `js-mode` and `js-jsx-mode` ([`d3b76f6`](../../commit/d3b76f615c5030a54ec3d74517a43c734700be50))
* Add `company` and `flycheck` support for multiple languages ([`4f71184`](../../commit/4f71184b0eab6b65bc7f57d615494f9d88afb020))
* Split moduels to each individual `config.el` file ([`d97ef5c`](../../commit/d97ef5c27090d65e3ba555c41443f5d877764493))
* Allow `lsp-mode` being active in file without the `project-root` being defined ([`b737a65`](../../commit/b737a6515e51b23dfb42c11ab2e49a54f48b9ca4))
* Add support for elisp keywords completion ([`19f8736`](../../commit/19f87363c77d15602f09dd358ce9237fe47ccca1))
* Extract modeline display to external package `jcs-modeline` ([`3cb0a63`](../../commit/3cb0a6304d4b95fca9a35b4efbf6434eae4494ed))
* Replace `toggle-quotes` with new package `cycle-quotes` ([`53c69a8`](../../commit/53c69a8532142b4ede97df717e07c29fff9e66a5))
* Remove unused command to toggle forward/backward sexp ([`4703c7f`](../../commit/4703c7fc81152f8818f5c4cba907d64980ae4c0c))
* Bind keys for `cycle-quote` and `cycle-slash` ([`8e5dec8`](../../commit/8e5dec85c47c4c9ff35e48df6bcbf317b687cc93))
* Replace `bool-flip` with new package `cycle-at-point` ([`e339971`](../../commit/e3399712e28730fa8b474032e6e17664f153ac68))
* Add new package `cycle-case-style` ([`6cad703`](../../commit/6cad7031a6b0ef58bbf74e74e020b272d53e4b19))
* Add eldoc support for CSS ([`9b9b4cd`](../../commit/9b9b4cd493ef35d31a78300c94cae9ad9e778c84))
* Add eldoc support for TOML ([`a0eb8ed`](../../commit/a0eb8ed975a9a7367ff94789d48f80ebb207dbf3))
* Use `literate-calc-mode` instead of self-defined calc command ([`691186d`](../../commit/691186d362f79838291779bac6bf50e76eebc015))
* Move back to use `use-package` ([`9d2ead7`](../../commit/9d2ead748c345b8edcf17fb9ee0e58b1b7464f09))
* Make echo all `re-mode` commands ([`26a37fa`](../../commit/26a37fa3b4b6e8f9125a4c8c133e8df03681a496))
* All major-mode for all .ignore files ([`a14a111`](../../commit/a14a111e8c59c08718f620a455a1c14e26f8779e))
* Add another file paths completion package, `company-paths` ([`a73cafc`](../../commit/a73cafcaf2f36192de06eeb3096a856f05e01113))
* Integrate `magit` ([`54953d7`](../../commit/54953d780fc9962d5d373cabb9a3c3a2f2c7629e))
* Add new package `vc-refrest` for better UX ([`91625bc`](../../commit/91625bc0ff66f773c8c040b3d56b2e64de8ba497))
* Add support for `epub` reader ([`04a1bd1`](../../commit/04a1bd1eba474f6949e96f2ec23befc96847de56))
* Add new package `free-keys` ([`9925b4c`](../../commit/9925b4ce6dd513843b07b1edae07565366bc8857))
* Add new package `npm-mode` ([`78ad3a6`](../../commit/78ad3a6cae3cc273c7c10695b1b5a5e5941ec686))
* Add new package `k8s-mode` for kubernetes configuration file ([`0db8edb`](../../commit/0db8edb596c264a9256ebf20ea74dd9b9e422a04))
* Add new module for languages `PureScript` ([`8ffb007`](../../commit/8ffb007bda2281a53b29782b898c2339033846ab))
* Add support for `PKGBUILD` file ([`b14cf80`](../../commit/b14cf80ecac042047e89dbb870ff79e75769bb8c))
* Add company support for Eask-file, `company-eask` ([`21655da`](../../commit/21655da7edbad6f85aed6cce67994948d831bfe0))
* Add eldoc support for Eask-file, `eldoc-eask` ([`fcd6247`](../../commit/fcd62477d4caa5d8c442568e37fd37d3309f0655))
* Remove `nhexl-mode`, and use `hexl-mode` instead ([`487e8ec`](../../commit/487e8ec100b915d82205216013c59dd4669e8902))
* Add annotation for `file-header` templates ([`5d32dbe`](../../commit/5d32dbe973a74cb918a32cd7489773eab247561c))

## 8.2.1
> Released Jul 22, 2022

* Add new package `sideline-blame` ([`c471339`](../../commit/c4713396e8937da498083bd3730f28bc114e3b47))
* Add new package `sideline-flymake` ([`322b5bb`](../../commit/322b5bb5123a60c5dbdcb11458d2c7eca92fe9a0))
* Add new package `echo-bar` ([`26c0bf7`](../../commit/26c0bf7c657a29368486aac112439a8507927b21))
* Change modeline color while debugging ([`c4f224f`](../../commit/c4f224fe926400089d8896f3a89fe545e12e66a2))
* Add new package `company-dockerfile` ([`ebb3556`](../../commit/ebb3556e9cb79f66f191e39dee8738d2cd1d33bc))
* Add new package `company-powershell` ([`3929062`](../../commit/39290625a468462d2ebaccc6a7d8cf4f13851311))
* Add new package `company-cmd` ([`b7ed059`](../../commit/b7ed05973c8b8b02a729734d1303ed1291dccba4))
* Add new package `company-makefile` ([`0e1d62a`](../../commit/0e1d62adddfb8babf9ab1478af58e1519d1f7a28))
* Add new package `company-autoconf` ([`cec8940`](../../commit/cec8940857a8bb434ecf0a37188cd6840602b70c))
* Add new package `company-coffee` ([`74e5a42`](../../commit/74e5a42eebaf812f266a4a508d059bed33c7986f))
* Refactor to use default function to get `*Messages*` buffer instead of declaring another variable explicitly in the global scope ([`ac7f079`](../../commit/ac7f0791c1f1e3de16a14831d07280957b77e244))
* Use default function to get `*scratch*` buffer ([`b6909cc`](../../commit/b6909ccb5f9f4fa6af211ac1e9bb0fd50afc7255))
* Replace built-in progress reporter with external library `prt` ([`aa0f6d8`](../../commit/aa0f6d8bbfe882e2ba4f783e6eca431b392c04cc))
* Use built-in `scratch-buffer` function instead ([`5a0957f`](../../commit/5a0957f214aa786b2ea82e41a2aff3052c63eda3))
* Fix function name `startup--get-buffer-create-scratch` to get scratch buffer `get-scratch-buffer-create` ([`8788c30`](../../commit/8788c3015561faf13c46e12a9e5febc5b1a983c2))
* Drop support for Emacs 28.x ([`7fa6129`](../../commit/7fa61292bd1553cee46884a98f4959933b55bb12))
* Diminish buffer `*Bug Help*` as default ([`cef9be2`](../../commit/cef9be24f401405fe58aed4496e6cac52099487b))

## 8.2.0
> Released Jun 21, 2022

* Drop support for Emacs 27.x ([`7fa6129`](../../commit/7fa61292bd1553cee46884a98f4959933b55bb12))
* Prevent whitespace cleanup for `text-mode` ([`56dca1a`](../../commit/56dca1ac48ef4e613c777b82bac1247513e6c024))
* Remove development package `ert-runner` ([`84ffcde`](../../commit/84ffcde033cab6073b3b2624b1c6f40260322a64))
* Remove development package `el-mock` ([`8fcc72f`](../../commit/8fcc72f1ec5fb723b3262070ad87095d42fea852))
* Fix bug for newly added packages from archives ([`6a92be2`](../../commit/6a92be2c8800b478bfb120a4489a7b1930235db0))
* Add new package `flycheck-eask`. ([`cd94bba`](../../commit/cd94bbabe3bb5e3878384afea030cb203e0e5050))
* Move minibuffer flx to external package [vertico-flx](https://github.com/jcs-elpa/vertico-flx) ([`151d8f2`](../../commit/151d8f20e2d5539add95deb892acfbb0f1638df7))
* Show `helpful` message while describing things at point ([`a06c8d1`](../../commit/a06c8d1edbb2380cf449a554dda0d10c3433ae3a))
* Add new package `suggest` ([`913f278`](../../commit/913f278afa145d46924cde5322506cd13dd43e9f))
* Inhibit redisplay on startup to speed up the startup time ([`9922190`](../../commit/9922190f2e53ccc9a400943140a3c5e6462bde5f))
* Add new package `company-shell` ([`6b5d79a`](../../commit/6b5d79a65d889222e42343325cf59790c78d97af))
* Add new package `company-glsl` ([`122b4a1`](../../commit/122b4a1b541acf7a810253d986d423e216a92ceb))
* Add new package `arduino-mode` ([`d4ffb8c`](../../commit/d4ffb8c6b5ca99619c601c26c85aa6063d6c1806))
* Add new package `on` and configured to improve startup speed ([`8c9f797`](../../commit/8c9f797492a74cdb30ba9ff6f7ee483c0683def5))
* Move init delay to `on-init-ui-hook` ([`705afe9`](../../commit/705afe9744fc3efb2ee005beb8db85e0a08b6fbe))
* Add new package `qml-mode` ([`e383783`](../../commit/e383783356143f9cb25ad315be6227823a7b44d7))
* Add new package `coffee-mode` ([`d7a9a99`](../../commit/d7a9a999e9dbd8c47fc3e2ac559517b776c0ac31))
* Add new package `crystal-mode` ([`72c3dba`](../../commit/72c3dba1d496fd85e96ea8f80da1caa3a56b2e06))
* Add new package `d-mode` ([`8bbb6fd`](../../commit/8bbb6fdce8351cccf22b59e08efa6a5969d8586e))
* Add new package `feature-mode` ([`76f0696`](../../commit/76f0696c15f60c44aced2a6c58a3039d7d214fcf))
* Add new package `git-assembler-mode`, `graphql-mode`, and `hexo` ([`444fd96`](../../commit/444fd96c9a5ebbdab04f74ae18f3a31978b3acfa))
* Add new package `recentf-excl` ([`1fde626`](../../commit/1fde6265b338d17efa5f47b908ad4b30c159d9da))
* Add new package `javap-mode` ([`6e6f53f`](../../commit/6e6f53f1f2cc05cef2f0065ad214f6d58d65eaa3))
* Fix conflict between `company-box-doc` and `dashboard` refresh ([`d49a031`](../../commit/d49a0310c3a28e1edabb6ba11436b046c701e63f))
* Fix popup tip showing previous/last description ([`45451bb`](../../commit/45451bb3460f0ffabb6af35c989bb3e6290e594c))
* Fix bury buffer proceed before `diminish-buffer-mode` will jump to unwanted buffer ([`184ba39`](../../commit/184ba39015b1b26342c13f8b107b69c389f0aeed))
* Add new packages `phpt-mode` and `robots-txt-mode` ([`eb4714b`](../../commit/eb4714b8280490fbf9c6f570479e6f3ba1a3a690))
* Remove package `blamer` and add new package `vc-msg` ([`067cfb9`](../../commit/067cfb98a52263754b7d4c4222d2bd9c8c4d0601))
* Improve `jcs-advice-add` macro ([`3ec7e94`](../../commit/3ec7e943ab0d035610ce6391e7f9d8ed8ff58cb1))
* Improve `jcs-add-hook` macro ([`48abd84`](../../commit/48abd848fba1b2cefa6c7086ca73ac679c3f83ec))
* Add new package `protobuf-mode` ([`6e4f275`](../../commit/6e4f27587b5a54a0a7bddc5e0a8cba2e008f1463))
* Remove tab and spaces converting util functions ([`5c92fa5`](../../commit/5c92fa525f486ef46e2e309ee5d47f48f5fef66a))
* Move built-in `multiple-cursors` functions to external package `vsc-multiple-cursors` ([`d12f928`](../../commit/d12f928aeb4f7765772576d6d0cf6329ffb081b6))
* Configure `multiple-cursors` for mouse click ([`454933b`](../../commit/454933b94926bab40c09810d49533c3eb3290239))
* Add new package `gcmh` ([`39e381d`](../../commit/39e381d56f12ba9866c71d1e7b2f31775ee25ed2))
* Disable `page-break-lines` in `emacs-lisp-compilation-mode` ([`b337b45`](../../commit/b337b4505ad14c8b756e810e9b1f7b65a598f786))
* Add new package `sideline-flycheck` ([`18cdd62`](../../commit/18cdd626331297f77255a8faa922f5dd199bc416))
* Add new package `docker` ([`bb0fbb6`](../../commit/bb0fbb611cce10a1ea4438b9bb0ab95110dd49cc))
* Configure compilation previous/next error keys ([`919a848`](../../commit/919a84801aef3631fa7dc496a8b997acaeeca0d2))
* Add new package `sideline-lsp` ([`b7b7c4e`](../../commit/b7b7c4ed9359412709f4fc0197005043b6d19fca))

## 8.1.0
> Released Apr 7, 2022

* Fix move file/directory with full path ([`f3f0c61`](../../commit/f3f0c61deb7aa3cc1b437a104c767e10050a73ef))
* Add new package `nim-mode`.
* Add new package `shift-number`.
* Moved custom face config for `markdown-mode` and `org-mode` to external packages ([`d92b571`](../../commit/d92b57181b358574245c0ec6c15985b49e4b83ec))
* Add basic support for Julia programming language.
* Add new package `dotenv-mode`.
* Add new package `docker-compose-mode`.
* Enabled `word-wrap-by-category` by default.
* Rename `jcs-with-eval-after-load-multiples` to just `jcs-with-eval-after-load`.
* Avoid macro `with-eval-after-load` to `leaf` if possible.
* Add new package `ascii-table`.
* Add new package `smart-comment`.
* Add new package `winum`, and replaced config select window commands with it.
* Add new package `log4e`.
* Remove config tabify/untabify save, and replace it with `whitespace-cleanup-mode`.
* Add new package `shell-pop`.
* Replace config built-in pop shell functionality with `shell-pop`.
* Disable save file message by default.
* Improved reopen buffer command's message. ([`2632444`](../../commit/263244468533c95fcf340b4603b8bb0e3ed00f1e))
* Limit lsp message to display in `*Messages*` buffer.
* Add file name when do `ffap` on `minibuffer` and `vertico`. ([`1df9e1b`](../../commit/1df9e1b4467423dc278ac4963cfd9d42e8d53d28))
* Add support for HLSL language.
* Add new package `message-clean-mode`.
* Add new package `buffer-menu-project`.
* Add new package `buffer-menu-filter`.
* Default select first candidate (highest score) after sorting while using minibuffer completion. ([`eb6486a`](../../commit/eb6486a3a9031421127fda03f89ede01d38ba409))
* Add new package `electric-indent-sexp`.
* Add new package `electric-cursor`.
* Remove implementation of `depends-mode` and `cross-mode`. ([`8a1500e`](../../commit/8a1500e4738453f757cb9d28a2c09136dedf6bcc))
* Remove file display function `jcs-html-preview` and `jcs-display-file` ([`bb47bce`](../../commit/bb47bcee49c2005c10c2b53bb62b923c74fb2741))
* Moved revert buffer module to external package, [vs-revbuf](https://github.com/emacs-vs/vs-revbuf). ([`73f88e1`](../../commit/73f88e137f7bf4155ff2a37d8b1c43248cf628c5))
* Add new package `whole-line-or-region`. ([`6a66781`](../../commit/6a6678143c8d03ccbb6fa479d8f711fceccc3e42))
* Moved Visual Studio editing experience to external package, [vs-edit-mode](https://github.com/emacs-vs/vs-edit-mode) ([`2db994d`](../../commit/2db994dbb567d40ccbcd987b09c5a5806db89ed5))

## 8.0.1
> Released Jan 29, 2022

* Hide PID from `lsp-mode` lighter.
* Add shorten lighter capability for `lsp-mode`.
* Remove unused symbol navigation functionalities.
* Move `jcs-frame.el` to `jcs-window.el` to speed up start up time.
* Replace function `jcs-bind-key` with macro `jcs-key-local`.
* Update `define-key` with new macro `jcs-key`.
* Installed new package `balanced-windows`.
* Installed new package `toggle-window`.
* Installed new package `transpose-frame`.
* Installed new package `minions`.
* Replaced package `diminish` with `minions-mode`; package `diminish` removed.
* Installed new package `moody`.
* Replaced mode line with `moody` instead of `powerline`.
* Switch from `ivy` to `vertico`.
* Installed new package `flx-style` for `completion-styles`.
* Installed new package `blamer`.
* Installed new package `mwim`.
* Change default `completion-styles` to `partial-completion`, so it goes easy on `company-mode` ([`132d4bc`](../../commit/132d4bc2de4f89cc37ebed2d8c9ff7617fcb26f4))
* Fix `undo-tree` jumping dedicated window after kill ([`bbf04f7`](../../commit/bbf04f799eeae8ceb7a7b8a891b459cebca0f2ec))
* Use `flx` for minibuffer `completion-styles` ([`e8041b1`](../../commit/e8041b1b5e9d2dd440894530ab908cff7d7b0354))

## 8.0.0
> Released Jan 6, 2022

* Correct save buffer logic for `css-mode`.
* Update EOL related key bindings.
* Remove all "is" major mode logic.
* Add `inhibit-modification-hooks` to with no redisplay macro.
* Add `display-buffer-alist` to with no redisplay macro.
* Move exteneral theme setitings to default theme packages, [vs-light](https://github.com/emacs-vs/vs-light-theme) and [vs-dark](https://github.com/emacs-vs/vs-dark-theme).
* Add `@` symbol as one of the company trigger symbol.
* Fix issue when add company backend will effect across all buffers.
* Remove package `flycheck-popup-tip`.
* Remove package `flycheck-pos-tip`.
* Disable logging when printing out error messages from `flycheck`.
* Move entire directory to `.emacs.d`. ([#32](../../pull/32))

## 7.1.0
> Released Dec 27, 2021

* Installed new package `asoc`.
* Replaced `quelpa` with `github-elpa`.
* Removed package `leaf-quelpa`.
* Inhibited building the dependency graph before init time.
* Installed new package `nginx-mode`.
* Installed new package `company-nginx`.
* Installed new package `ada-mode`.
* Removed functionality to open update log in project.
* Removed functionality to open todo file in project.
* Delay init instead of registering in the first pre-command hook.
* Installed new package `lsp-metals`.
* Installed new package `lsp-sonarlint`.
* Installed new package `lsp-tailwindcss`.
* Disable display `HUD`, `MULE` and `buffer-size` information in mode line by default.
* Installed new package `sort-words`.
* Removed package `atl-markup`.
* Removed package `atl-long-lines`.
* Installed new package `calfw`.
* Simplify templates/snippets utility function names.
* Added new hook, when after theme loaded.
* Disabled `highlight-indent-guides` by default inside terminal.
* Replaced `jcs-emacs-ready-p` flag with `after-init-time` instead.
* Installed new package `cargo-mode`.

## 7.0.0
> Released Nov 17, 2021

* Configured `company-box` so it's compatible to old `company-quickhelp` configuration.
* Installed new package `logms`.
* Moved emoji company backend fo just `markdown-mode`.
* Removed unnecessary package version calculation.
* Installed new package `company-emojify`.
* Removed legacy code from `logging` module.
* Added improvements for `ReactJS` and `React Native`'s default templates.
* Added ansi color support for compilation buffer.
* Enhance rule for `rjsx-mode` detection for JavaScript major modes.
* Fix url browsing functionality with key <kbd>Ctrl</kbd>+<kbd>Enter</kbd>.
* Revised minify and prettify contents with region.
* Removed `elisp` directory from `.emacs.d`, replace with [quelpa](https://github.com/quelpa/quelpa).
* Installed new package `editorconfig`.
* Added `Project` to buffer menu list.
* Added function to track the opened projects.
* Added capability to show project name wiht `powerline`.
* Improved `buffer-menu` module with project capability.
* Replaced `use-package` with `leaf`.
* Improved for built-in package, `comint` related to `compilation-mode`'s input.
* Added custom bind key function.
* Removed config for package, `reload-emacs`.
* Improved a bit of startup time.
* Reconstructed function modules to speed up startup time.
* Customized `dashboard` so it no longer using `page-break-lines` to show separators.
* Removed package `origami.el`.
* Installed new package `ts-fold`.
* Terminated `undo-tree` visualizer buffer after reopening the buffer.
* Limited `powerline` to apply only for the valid `mode-line-format` variable.
* Installed new package `meta-view`.
* Installed new package `eldoc-meta-net`.
* Installed new package `company-meta-net`.
* Installed new package `hl-preproc`.
* Introduce new variable `jcs-log` for controlling the logger module.
* Installed new package `flx-rs`.
* Replaced scoring algorithm from `flx` to `flx-rs`.
* Added support to delete dynamic module packages.
* Installed new package `csv-mode`.
* Removed `indent` support for next/previous key types.
* Added Notepad++ style indent block commands.
* Removed unused vim layer.

## 6.5.0
> Released Jul 17, 2021

* Used default `undo` when `undo-tree` not enabled.
* Fixed logic while installing pinned packages as dependency.
* Suppressed warning message while checking light/dark theme in `after-init-hook`.
* Optimized performance for window utility functions.
* Added new utility macro, try run/execute in repetitions.
* Fixed project unsearchable issue when projct contains sensitive keywords.
* Selected line endings will only ask once for `.sh` files.
* Added pinned package list for archive source binding.
* Enable package `origami` by default.
* Added `region` face customization for theme configuration.
* Customized theme face so it's closer to Visual Studio IDE's theme.
* Made improvement for `dashboard` utility functions.
* Config dashboard for `bookmarks` support.
* Removed package `magit`.
* Applied yank workaround for `makefile-mode`.
* Added utility function for identify a invalid buffer.
* Ensure kill invalid buffer occurs unless shown in multiple windows.
* Enabled `display-fill-column-indicator-mode` as default behaviour.
* Avoid tracking recent files when trigger goto definition.
* Support full line when removing item from `dashboard`.
* Installed new package `elm-mode`.
* Installed new package `keytar`.
* Configure company icon margin function base on theme color.
* Added theme customization for `company-mode`.
* Fixed `undo-tree` visualizer buffer triggers in incorrect buffer.
* Aborted `company-mode` while kill whole line command.
* Replaced focus in/out hook to `after-focus-change-function`.
* Configure horizontal scroll variables.
* Make sure to install missing dependency while on start-up.
* Fixed issue project item not removing from `dashboard`.
* Added new package `logview`.
* Fixed the calculation of the active package list.
* Removee indent level configuration from `json-mode`.
* Fixed guess path logic from dashboard item alists.
* Fixed not correct backspace/delete indent level.
* Ensure `highlight-indent-guides-mode` for `.yaml` and `.xml` file.
* Added utility function buffer show in list.
* Only revert necessary buffers to save performance on `focus-in` event.
* Used `vc-mode` instead of `magit` while updating `feebleline`.
* Added utility function replace non-displayable character.
* Added new package `lsp-grammarly`.
* Fixed missing comma prompt from command `jcs-package-autoremove`.
* Removed package `reload-emacs`.
* Removed package `test-sha`.
* Added template and basic configuration for AppleScript file.
* Installed new package `lsp-ltex`.
* Stopped guessing indent level for python file.
* Installed new package `flycheck-languagetool`.
* Make internal border wider for package `pos-tip`.
* Configure `so-long`, disabled minor mode list.
* Added `evaluate`/`byte-compile`/`load-file` utility functions for elisp development.
* Removed redundant lsp customization about [lv](https://melpa.org/#/lv) package.
* Added new feature, install custom `tree-sitter` queries.
* Removed package `company-quickhelp`.
* Installed new package `company-box`.

## 6.4.2
> Released Feb 24, 2021

* Install new package `applescript-mode`.
* Install new package `turbo-log`.
* Change source for package `docstr` from [quelpa]() to [melpa]().
* Ensure lightblub image loaded for module `lsp-ui-sideline`.
* Bindy magit refresh for configuration generic reopen key.
* Revert `tree-sitter` highlighting queries' customization.
* Add more support on package `magit`.
* Fix not readable character from `lsp-modeline`.
* Stop tracking recent files from peeking definition.
* Fix utility function's logic for inside the string block.
* Add my own `tree-sitter` queries files for highlighting support.
* Remove css face customization and replace with `tree-sitter` highlighting.
* Install new package `el-mock`.
* Install new package `ert-runner`.
* Install new package `undercover`.
* Changed `lsp-python-ms` to `lsp-pyright` for default python's language server.
* Added function `jcs-print` for general printing purpose.
* Set variable `warning-minimum-level` default to `:emergency`.
* Revert remove trailing spaces `save-buffer` function in `markdown-mode`.
* Removed package `neotree`.
* Use package `treemacs` instead of `neotree`.
* Fixed getting the starting comment symbol from point.
* Shortern the `is-contain` utility functions.
* Added creator environment settings.
* Fixed closing parenthesis still get indent issue.
* Remove `css` return key and it's binding.
* Remove `web` return key and it's binding.
* Bind function `newline-and-indent` to default return key.
* Fixed `ffap-guesser` logic while using `dashboard`.
* Fixed incorrect `save-buffer` function from `company-mode`.
* Moved complicated document string asterisk module to external package `docstr`.
* Moved compilcated document string module to external package `docstr`.
* Enhanced grabbing symbol on both comment `start` and comment `end`.
* Removed dashbaord refresh limitation due to buffer shown.
* Changed VS-like closing curly parenthesis action to all generic closing parenthesis.
* Add key advice add/remove utility functions.
* Ensure all programming major mode has valid variable `tab-width` defined.
* Add character `!` as word entry for `rust-mode`.
* Removed customize module `jcs-yaml` file.
* Improved package delete without complaining miss dependencies.
* Inhibit `dashboard` refresh process while minibuffer is active.
* Refresh `dashboard` after minibuffer is exited.
* Added utility function for checking minibuffer is prompting.
* Added TOML file configuration with `conf-toml-mode`.
* Installed new package `lsp-sourcekit` for LSP Swift support.
* Diminished the buffer `*ert*` as default behaviour.
* Implemented package autoremove for configuration's dependency graph.
* Moved package archive for package `indent-control` from [quelpa]() to [melpa]().
* Set default `.unityignore` major mode to `gitignore-mode`.
* Improved message information while file renaming.
* Set default `.dockerignore` major mode to `gitignore-mode`.
* Set default `.npmignore` major mode to `gitignore-mode`.
* Fixed logic for select multiple files in current directory.
* Fixed conflict from `neotree` to `dashboard`.
* Fixed error by passing not existing path when getting file content.

## 6.4.1
> Released Jan 12, 2021

* Installed new package `scrollable-quick-peek`.
* Implemented peek definition functionality.
* Fixed issue LSP not restart after reopening the buffer.
* Enabled `tree-sitter-hl-mode` as default behaviour.
* Configured `tree-sitter-hl` faces for `light` and `dark` theme.
* Removed package `preproc-font-lcok` and it's configuration.
* Implements self-wrap preprocessor font lock implementation.
* Enabled `tree-sitter-mode` as default behaviour.
* Added triple slash header template.
* Started programming language F# support.
* Fixed indent issue for langauges that do not have indentation level definition.
* Set default indentation level for `ruby-mode` to `2`.
* Simplify `setq-local` operator within each module.
* Added save scroll conservatively macro.
* Added save window layout/settings macro.
* Fixed recentering redisplay issue while upgrading packages.
* Start with `tree-sitter` support.
* Fixed dashboard not reverting while on `focus-in-hook`.
* Fixed jumpy dashboard while visiting it's buffer.
* Simplify the dashboard startup info.
* Fixed smart previous/next line logic while during line.
* Customized some `org-mode` file faces.
* Enabled fontify code blocks natively for `markdown-mode` as default behaviour.
* Added reveal truncate path from dashboard when using counsel.
* Fixed dashboard refresh, respect to last visisted valid buffer.
* Ensure `dashboard-ls` will respect the the dedicated default directory path.
* Added centering the `dasboard` buffer width window resize hook.

## 6.4.0
> Released Dec 22, 2020

* Fixed recording recent files while installing issue. ([#23](../../pull/23))
* Added generic buffer list filter utility function.
* Added Emacs Lisp project load path for future Emacs Lisp Project Development.
* Enabled always defer from `use-package` as default behaviour.
* Add configuration for package `docstr`.
* Fixed refresh dashboard logic when switching buffer including killing buffer.
* Ensure valid buffer buffer when refreshing dashboard buffer.
* Removed self-customized `docstr` configuration.
* Fixed logic for insert/delete spaces by indent level.
* Fixed feebleline getting indentation level.
* Added under project utility function in `project` module.
* Installed new package `indent-control`.
* Moved indent level management to external package `indent-control`.
* Changed archive source for `ivy-file-preview` from `quelpa` to `melpa`.
* Installed new package `diff-hl`.
* Disabled message log when `yank`.
* Installed new package `highlight-escape-sequences`.
* Enabled drag and drop region as default behaviour.
* Fixed isearch not recenter issue.
* Fixed push button from `*Help*` buffer not recenter issue.
* Added safe way to revert all buffers when on focus.
* Added return buffer by using file path in utility module.
* Fixed windows state record/restore functionality by using get buffer by path.
* Added check for reverting buffers using file edited externally strategy.
* Removed package `projectile.`
* Removed package `counsel-projectile.`
* Installed new package `project`.
* Switched dashboard project support from `projectile` to `project`.
* Added virtual buffer list utility function.

## 6.3.1
> Released Dec 9, 2020

* Added [celpa](https://celpa.conao3.com/) to package archives list.
* Installed new package `quelpa-use-package`.
* Added mouse scroll configuration for better mouse scrolling user experience.
* Unbind `up`/`down` keys in `dashboard` buffer and it's mode.
* Set initial buffer for Emacs daemon.
* Make refresh exhibit find file while entering `./` present directory.
* Configured `diminish-buffer` mode list.
* Add `early-init` module for Emacs version after `27.1`.
* Installed new package `lsp-dart`.
* Installed new package `lsp-docker`.
* Installed new package `lsp-haskell`.
* Installed new package `lsp-latex`.
* Installed new package `lsp-mssql`.
* Installed new package `lsp-pascal`.
* Installed new package `lsp-python-ms`.
* Add `modablist` package's configuration.
* Add limitation to use `buffer-wrap` for certain `major-mode`s.
* Moved `company-keywords` backend into a much prior place.
* Installed new package `jenkinsfile-mode`.
* Added configuration for Jenkinsfile.
* Diminished the buffer `*Flymake log*` as default behaviour.
* Diminished the buffer `*wclock*` as default behaviour.
* Added `wident`/`narrow` `tabulated-list` column default key bindings.
* Installed new package `docstr`.
* Replaced `jcs-docstring` to external package `docstr`.
* Add configuration `flycheck-grammarl` package.
* Minor clean up for `utility` module.
* Changed previous/next line type from `indent` to `smart`.
* Updated smart previous/next line logic.
* Add `newline` advice for smart indent return.
* Simplify document string module.
* Changed comment style from `REM` to `::` in `bat-mode`.
* Updated re-build dependency graph logic.
* Updated folding/unfolding logic using `origami`.
* Fixed `daemon` startup issue.
* Added `backward`/`forward` deep 1 level sexp.
* Bind `backward`/`forward` deep 1 level sexp to global key map.
* Bind `backward`/`forward` unlimited levels sexp to global key map.

## 6.3.0
> Released Nov 5, 2020

* Extract open and close balanced expression to list.
* Fixed toggle move to balanced expression logic for next character point.
* Installed new package `fill-page`.
* Added line number at pos relative utility function.
* Revised to improve UX when folding using `origami`.
* Fixed the startup error triggered by evaluating `web-mode`.
* Fixed `lua-mode` highlighting issue, see [lua-mode/172](https://github.com/immerrr/lua-mode/issues/172).
* Support programming language `Scala` document string.
* Support programming language `Rust` document string.
* Configure `csharp-mode` for `c-markup` highlighting.
* Removed self declared `c-style` for `C` related programming languages.
* Installed new package `atl-long-lines`.
* Added toggle move to balanced expression (sexp).
* Renamed all `-func` suffix module to it's name.
* Customized `origami` folding face.
* Fixed `css` variable name face applied globally issue.
* Added configuration for `dashboard` shortcuts.
* Removed `org-mode`'s refresh table functionality.
* Added customize faces entry for `markdown-mode`.
* Changed text banner customization for package `dashboard`.
* Diminished the buffer `*VC-history*` as default behaviour.
* Improved better default face for `markdown-mode`.
* Installed new package `0xc`.
* Organized/Removed obvious package dependency from pre-install package list.
* Fixed missing path issue while using guess path.
* Fixed marking whole buffer logic bug.
* Bind `expand-region` keys to global key map as common use.
* Added resolve expand region makring after operation commands.
* Added `type` compare string utility function.
* Changed from `strict` to `type` when counting shown buffer.
* Customize `rjsx-mode`'s highlight faces.
* Installed new package `expand-region`.
* Added `backtrace-mode` configuration.
* Set default to unfold for `org-mode`.
* Changed source for package `impatient-showdown` from `quelpa` to `melpa`.
* Implemented `util` argument when walking through windows/frames.
* Diminished the buffer `*preview-it` as default behaviour.

## 6.2.6
> Released Oct 14, 2020

* Config `auto-rename-tag` package for disabled commands and `minor-mode`s.
* Make rever all buffers after replace commands.
* Added `Advices` section for declaring generic advices.
* Make recenter after all `ivy-searcher` commands.
* Implemeneted auto configure for `C/C++` include path on Windows.
* Organized code with better `@` code separator.
* Implemented `guess path` for possible auto config environment path utility function.
* Diminished the buffer `*Flycheck errors` as default behaviour.
* Make generic log list able to log `hash-table`.
* Installed new package `company-c-headers`.
* Make log list compatible to `array` and `vector` data structure.
* Minor configure for package `keypression`; ignore `switch-frame` and other certain commands.
* Bind <kbd>Shift</kbd>+<kbd>u</kbd> instead of <kbd>u</kbd> for upgrading all packages key.
* Added global ivy minibuffer enabled flag for other use.
* Make frame/window size respect to `ivy-height`.
* Re-configure `company-backends` to specific major mode.
* Added `append` argument when modifying `auto-mode-alist`.
* Diminished buffer menu for `re-builder` buffer.
* Implemented mimic window config when doing `jcs-same-file-other-window` command.
* Implemented same file other window back to original window config functionality.
* Minor fix for maybe kill function for `must-kill`/`virtual` buffer.
* Defined `multipe-cursors` cancel command list.
* Diminish `auto-fill-function`'s lighter.
* Changed `jcs-save-excursion` to macro instead of function.
* Implemented automatically align org table after changes.
* Reuse process reporter messaging system to title when rebuild dependency graph.
* Renamed `oop` module to `docstring` for better naming.

## 6.2.5
> Released Sep 24, 2020

* Allowed virtual buffer to be buried instead of killing the buffer.
* Multiple minor bug fix regarding to variable naming issue.
* Moved error/backtrace module to generic module, `jcs-function.el`.
* Standardize maybe kill buffer logic by separating exception list.
* Added default `sleep-for` function for configuration's own use.
* Make adjust scroll from `ivy` to `window` module.
* Implemented max/min list utility functions.
* Bind rebuild dependency graph function to after package execute command.
* Implemented rebuild dependency graph function for organizing 3rd party packages.
* Removed package `fill-page` from manual installation.
* Removed package `un-mini` from manual installation.
* Instead of `fill-page`, use adjust scroll instead for `ivy-mode`.
* Extracted minibuffer configuration to a single file, `jcs-minibuf.el`.
* Extract environment separator character to a single variable.
* Installed new package `un-mini` manually.
* Enabled `un-mini-mode` as default behaviour.
* Fixed `feebleline` incorrect file name issue.
* Installed new package `better-scroll`.
* Correct configure `auto-highlight-symbol`'s faces.
* Placed `recipes` folder for manually installed packages.
* No longer need to specify manually installed packages in the configuration.
* Installed new package manually `fill-page`.
* Enabled `fill-page` globally be default.
* Installed new package manually `ivy-file-preview`.
* Enabled `ivy-file-preview-mode` globally by default.

## 6.2.4
> Released Sep 11, 2020

* Diminished `keypression-mode`'s lighter.
* Diminished `hi-lock-mode`'s lighter.
* Clean up util frame logic by confirming the parent frame existence.
* Implemented smart version of `org-cycle`.
* Changed with eval after load multiple to `macro` instead of `function`.
* Added snippet to `ess-r-mode` for `R`.
* Added snippet to `powershell-mode` for `PowerShell`.
* Reimplemented flatten from utility module.
* Config package `ivy-searcher`.
* Fixed `feebleline` branch not been updating after VC status changed.
* Installed new package `eshell-syntax-highlighting`.
* Installed new package `powershell`.
* Changed install source for package `license-templates` from `quelpa` to `melpa`.
* Removed packages `ag`, `wgrep`, and `wgrep-ag`.
* Replaced all `ag` and `wgrep` functionalities with `searcher` instead.
* Changed default `dumb-jump` selector to `ivy` and not `helm`.
* Removed broken `post-command-hook` for `lsp-mode`.
* Several bug fixed while extracting LSP config to isolated file.
* Installed new package `ivy-searcher`.
* Split up the LSP config to isolated module.
* Installed new package `searcher`.
* Fixed logic for single window workflow's compatibility with `undo-tree`.
* Implemented reset theme for `tabbar`, from `centaur-tabs`.
* Minor changes to use environment/platform separator in other use case functions.
* Simplify manually install package system from `quelpa`.
* Simplify message logging when installing package through `quelpa`.
* Uninstalled package `markdown-preview-mode`.
* Implemented markdown preview using `impatient-mode`.
* Added `*httpd*` to diminish buffer mode list.
* Installed new package `impatient-showdown` manually.
* Merged `impatient-showdown` with normal `impatient-mode` activation.
* Installed new package `markdown-preview-mode`.
* Implemented `showdown` to display github flavor markdown to `markdown-preview-mode`.
* Fixed not deleting temporary file after web server shutdown from `markdown-preview-mode`.
* United indentation level `setter`/`getter` functions.
* Fixed minor mode enabled utility function's logic.
* Implemented no log macro utility function.
* Converted `mute`/`unmute` apply utility functions to macro.

## 6.2.3
> Released Aug 26, 2020

* Fixed switching buffer after quit `undo-tree`'s visualizer with single window.
* Added sort symbols function as feature enhancement.
* Set `emojify-mode` to ON as default in `markdown-mode`.
* Fixed broken `isearch-project` with function name advice changed.
* Fixed ignore directories issue with `f-slash`.
* Installed new package `elisp-demos`.
* Simplify dashbaord nav key functions.
* Added global separator environment variable.
* Added `*Kill Ring*` buffer to diminish buffer list.
* Added text file identifier utility function.
* Added `*Local Variables*` buffer to diminish buffer list.
* Fixed `esup` package initialize issue.
* Added `*ESS*` to diminish buffer list.
* Set default diminished to `fill-page-mode`.
* Fixed `counsel` find file other window by just getting buffer object directly.
* Installed new package `fountain-mode`.
* Added support for Fountain markup language.
* Replaced `region-occurrences-highlighter` from local to global activation.
* Changed install source from `quelpa` to `melpa` for package `atl-markup`.
* Ensure no project running when active `auto-read-only`.
* Configured `diff-mode` for patch file editing.
* Implemented the default save buffer method depends on major mode.
* Revised save all buffers method for better user experience.

## 6.2.2
> Released Aug 13, 2020

* Added global `quit` command advice.
* Added global process reporter instance utility functions.
* Fixed display issue when upgrading packages using `quelpa`.
* Minord fixes for web development environment.
* Mark `auto-read-only` due to `quelpa`'s activation.
* Added functionality to check needed upgrade packages to manually installed packages.
* Added config for package `most-used-words`.
* Fixed `buffer-wrap` recenters window position to center issue.
* Fixed enable/disable truncate lines with `1` and `-1`.
* Uninstalled manually installed package `auto-truncate-lines`.
* Installed new package manually `atl-markup`.
* Removed verbose C/C++/Objective-C insert header template functions.
* Edited default C source header template.
* Organized code with less duplicate code.
* Installed new package `most-used-words`.
* Installed new package manually `test-sha`.
* Updated the `Line` utility module to use built-in solutions.
* Installed new package manually `license-templates`.

## 6.2.1
> Released Jul 23, 2020

* Installed new package `nix-mode`.
* Start supports for expression language `Nix`.
* Applied company abortion after switching window.
* Implemented scroll up/down goto center line functions including other window's version.
* Fixed `undo-tree` visualizer not quiting correctly after the parent buffer is killed.
* Fixed `line-reminder` signs for `linum-mode` when running in terminal.
* Installed new package manually `better-scroll`.
* Installed new package `auto-read-only`.
* Removed self-customized auto read only functionality and replace with `auto-read-only` package.
* Removed built in auto truncate lines functionalities.
* Installed new package manually `auto-truncate-lines`.
* Added prettify supports to `xml-mode` related major modes.
* Added prettify supports to `html-mode` related major modes.
* Installed new package `gitignore-templates`.
* Added functionality to ask gitignore template when creating new file in `gitignore-mode-hook`.
* Updated valid insert header function to keyword base instead of optional base.
* Installed new package `groovy-mode`.
* Start supports for programming language `Groovy`.
* Implemented docstring functionalities to programming language `Groovy`.
* Added `*Checkdoc Status*` to diminish buffer list.
* Added `*Package-Lint*` to diminish buffer list.
* Added new template branch for JSX and React JS/Native files.
* Added ask source for `JavaScript` template.
* Added ask source for `JavaScript XML` template.
* Fixed missing `iedit` when calling `yank` key.
* Fixed find file other window with opening the same filename error.
* Added `GC` control for `company-mode` and it's activation.
* Fixed after command `yank` kills `iedit-mode` issue.
* Fixed missing require when first time `overwrite-mode` activation.
* Added `*Warnings*` to default diminish buffer list.
* Added symbol dash `-` to default syntax table to programming mode.
* Fixed `feebleline` error handling cause performance issue.
* Added `counsel` find file command listener for current and up one directory.
* Removed exit `js2-minor-mode` from global post command hook.
* Fixed `*scratch*` buffer maybe kill key's logic.
* Organized `save` module with merging validity save infront.
* Added kill thing at point util function.
* Added loading `yasnippet-snippets` entry in `company-mode` activation.
* Installed new package `helpful`.
* Added helpful help/content buffer to default diminish buffer mode list.
* Removed self customized comment/uncomment function due to `line-reminder` updates.
* Added more company's backends to default usage.

## 6.2.0
> Released Jul 10, 2020

* Fixed `company-fuzzy` activation in `lsp-mode`.
* Updated re-builder module functions.
* Updated `company` default minimum prefix length from `2` to `1`.
* Uninstalled package `company-lsp` due to deprecated reason.
* Removed `company-lsp` config when `lsp-mode` enabled.
* Replaced `company-lsp` with `company-fuzzy` when `lsp-mode` enabled.
* Updated message maximum log limit to 10 times higher than default.
* Added backtrace constain rules for it's dedicated window/buffer.
* Bind eval related keys to global key map.
* Installed new package `keypression`.
* Use add advice to `overwrite-mode` instead of function wrapper.
* Changed `multiple-cursors`'s fake cursors when `overwrite-mode` is on/off.
* Updated `transwin` package from `quelpa` to `melpa`.
* Fixed oop highlighting from docstring type.
* Added new target to diminish buffer list, `*Apropos*`.
* Installed new package `ialign`.
* Ranmed template files with language prefix.
* Changed `Other` option to `ActorComponent` for proper naming when creating Unreal C++ file.
* Relocated Unreal C++ template files.

## 6.1.5
> Released Jun 28, 2020

* Implemented create parent directory when find file not found.
* Implemented remove virtual parent directory when killing virtual buffer.
* Added more Unreal C++ template files.
* Added ask for Unreal C++ file type when creating Unreal C++ file in `c++-mode`.
* Added customize face for `feebleline`'s project name.
* Fixed `c` and `c++` extension using `regex` constant check.
* Installed new package manually `transwin`.
* Fixed `ivy` logic when trying to find files in home directory.
* Updated `feebleline`'s UX when using terminal for window divider.
* Updated to record `*scratch*` starting content.
* Fixed retrieve message erase buffer logic.

## 6.1.4
> Released Jun 25, 2020

* Fixed maybe kill buffer logic by not using regex for buffer check.
* Updated buffer check with strict for not using regex check.
* Installed new package `github-browse-file`.
* Added toggle diminish buffer mode key to global map.
* Added valid buffer boolean check utility function.
* Updated buffer menu return logic.
* Updated maybe kill buffer function to switch to valid buffer logic.
* Updated diminish buffer list, `*Backtract*`, `*Compile-Log*`, `*Help*`.
* Cleaned up generic maybe kill this buffer function.
* Fixed dashboard is missing after maybe kill function is called.
* Fixed ivy skip input line logic.
* Fixed `feebleline` string type error from prepare operations.
* Updated `feebleline` for better priority.
* Updated `jsx` default template for React.js.

## 6.1.3
> Released Jun 14, 2020

* Updated `feebleline` format to better standard/format.
* Renamed repo to just `jcs-emacs`.
* Improved certain modes' default template files.
* Fixed csharp-like mixed c-like comment/docstring return logic.

## 6.1.1
> Released Jun 12, 2020

* Fixed goto dashboard command when dashboard buffer already shown issue.
* Added minimum required Emacs version check.
* Installed `csproj-mode` for supporting `.csproj` type of file.
* Added auto save config to env module.
* `json-mode` doesn't defined tab-width, use default instead.
* Fixed window name matching issue in shell module.
* Implemented source control information in `feebleline`.
* Changed `feebleline` using cache for better speed and performance.
* Fixed `cc-mode` docstring with keywords.
* Enabled JSX docstring with `rjsx-mode`.
* Added Unreal C++ template information functions.
* Added Unreal C++ scripting template files for `header`/`source`.
* Implemented ask for which header insertion in `c++-mode`.
* Declared c/c++ header and source extension list.
* Fixed `counsel`/`counsel-projectile` find file other window logic.
* Added `isearch` configuration by showing the match count.
* Removed `preproc` face from `face` module.
* Removed `preproc` function, no longer needed.
* Removed `oop` highlight faces for fixing OOP docstring implementation.
* Removed manually installed package `shift-select`.
* Fixed mark whole buffer action after switching window.
* Fixed `ivy` skipping input selection logic.
* Enabled `auto-rename-tag` for certain web related modes.
* Improved `JSX` coding experience by supply more helper plugins.
* Fixed performance issue when reload active mode.
* Fixed performance issue when enable/disable truncate lines in `web-mode`.
* Added auto install dependencies flag.
* Config now automatically install needed dependencies after first startup.
* Added safe active LSP util function.
* Semi-fixed performance when refresh dashboard.
* Split buffer shown `-count` and `-p` function in window module.
* Fixed `undo-tree` slow response when multiple undo/redo actions.
* Implemented `lsp` connected flag util function.

## 6.1.0
> Released May 16, 2020

* Implemented find directories ignore directories function.
* Implemented find files ignore directories function.
* Fixed `make`/`run` script find directories/files time spend too long issue.
* Installed `rjsx-mode` for supporting `jsx` file.
* Added JSX default snippets.
* Added condition to limit `lsp-deferred` is called.
* Minor fixed for `cc-mode` indent block settings.
* Config `counsel`'s `find-file` preselect flag.
* Fixed deep directory tree when using `counsel` find file action.
* Implemented `inc`/`dec` string distance level for `multiple-cursors` similarity.
* Installed new package `dashboard-ls`.
* Removed package `focus`.
* Implemented safe refresh `dashboard` functionality.
* Fixed OOP docstring functionalitiy when no parameters.

## 6.0.6
> Released Mar 23, 2020

* Changed source for package `emoji-github` from `quelpa` to `melpa`.
* Updated `diminsh-buffer` list for more of the LSP buffer.
* Installed new package `lsp-java`.
* Installed new package `lsp-origami`.
* Moved more mode autoload from `jcs-cc-mode` to `jcs-mode` file.
* Added the lsp ui doc delay when safely show lsp ui doc tooltip.
* Installed new package `manage-minor-mode-table`.
* Installed new package `buffer-wrap`.
* Installed new package `masm-mode`.
* Implemented `asm` mode behaviour for all Assembly Language related `major-mode`s.
* Changed source for `company-quickhelp-terminal` package fro `quelpa` to `melpa`.
* Fixed not refresh issue header string from `buffer-menu`.
* Fixed `ivy` overlap logic.
* Changed using minor mode for `company-quickhelp-terminal` package.
* Installed new package `company-emoji`.
* Installed new package `emoji-github` manually.
* Installed new package `emojify`.
* Fixed buffer wrap can't correct goes to corresponding first line isssue.
* Use local variable for `tabulated-list`'s header string.
* Diminish mode `buffer-wrap-mode` for `buffer-wrap` package.
* Switch `ffmpeg-player` from source `quelpa` to `melpa`.
* Removed `dashboard` mute when insert project sections.
* Installed new package manually `buffer-wrap`.
* Removed package manually `tabulated-list-wrap`.
* Installed new package `command-log-mode`.
* Mute dashboard insert project log.
* Fixed load `eww` issue from emacs version `27.0.60`.
* Installed new package `flycheck-grammarly`.

## 6.0.5
> Released Feb 16, 2020

* Implemented `neotree` recording the last window.
* Tweak `neotree` customizable variables' value.
* Changed `output`/`compilation` buffer maybe kill buffer default action to
 change to other output buffer as higher priority.
* Implemented switch to next window in height utility function in window module.
* Removed package `sr-speedbar`.
* Installed new package `neotree`.
* Replace config from `sr-speedbar` to `neotree`.
* Implemented automatically refresh `neotree` using timer.
* Fixed return type with empty string in `oop` module.
* Removed manually installed package `vs-light-theme`.
* Removed manually installed package `vs-dark-theme`.
* Installed new package `vs-light-theme`.
* Installed new package `vs-dark-theme`.
* Implemented docstring for `Go` programming language.
  - Support two way to comment document string.
* Fixed `ivy` find file logic with regular expression.
* Added expression face for `feebleline` lsp.
* Fixed region delete for smart delete word and capital word.
* Indent `yank` in `python-mode` gives weird result, use normal `yank` instead.
* Chnaged the constant keywords' face in `go-mode`.
* Implemented LSP information to feebleline.
* Added own `save-excursion` function.
* Fixed indent error while untabify/tabify buffer in `go-mode`.
* Fixed `company-fuzzy` renable issue with `lsp-mode`.
* Disabled debug on error while LSP server is active.
* Update `go` programming language default template.
* Reimplemented OOP docstring module, mainly for clean up.
* Fixed buffer/file name prompting error while reopening the file.
* Installed new package => `manage-minor-mode`.
* Fixed requiring `cl` using `loop` in util module.
* Ignored `lsp` buffer with weather the dash `-` exists or not.
* Removed `yascroll` issue with Emacs 27.
* Fixed `feebleline` compatbile with `lsp-mode` ignore case issue on `(buffer-name)`
 we use `(buffer-file-name)` beforehand.
* Updated `jQuery` snippet in `html-mode`, not slime use normal minified version.
* Fixed `null` face highlighting regular expression.

## 6.0.4
> Released Jan 9, 2020

* Update upgrade manually installed package logic.
* Installed new package manually => `tabulated-list-wrap`.
* Fixed visualize `undo-tree` on the other window doesn't work with `lsp-ui-doc`.
* Fixed hanging when execute `lsp--execute-command` function from `lsp-ui-sideline`.
* Fixed opening `css` virtual buffer with virtual directory issue with loading `eww`.
* Fixed some of the regular expression faces in `typescript-mode`.
* Implemented describe path info at point function and embedded to describe thing at point.
* Changed python class' template format.
* Added the `keywords` to the template config file.
* Installed new package => `grammarly`.
* Added title source for package missing prompt.
* Removed manually installed package => `ivy-resize`.
* Fixed buffer menu logic fit the `search`/`filtering` when multiple buffer menu buffer exists.
* Fixed buffer menu other window that doesn't goes to line `2` if the header appears.
* Fixed display not ready while `filtering`/`searching` in buffer menu.
* Added wrapping functionality for buffer menu mode.
* Established template default template's naming convention.
* Improved buffer menu `searching`/`filtering` user experience.
* Fixed buffer menu refresh logic after killing.
* Added wrapping to buffer menu buffer.
* Added switch to output buffer utility function for development use.
* Added get buffers utility functions by using `regexp` and `string`.
* Change `define-it` package from manually installed to automatically installed using `melpa`.
* Installed new package => `define-it`.
* Fixed `undo`/`redo` logic and work with `lsp-ui`.
* Added no advice other window flag for other function that doesn't want to configure
 buffer in window while switching window/frame.

## 6.0.3
> Released Dec 28, 2019

* Change default `define-it` to `'view` instead of `'pop`.
* Fixed `lsp-ui` show prompting error while switching windows.
* Implemented `lsp-ui` show doc anytime functionalities.
* Increase standard string distance level from `8` to `20`.
* Enhanced the `multiple-cursors`'s similar functions string comparison algorithm.
 Make improvements for the use of the `multiple-cursors` similar occurrence command.
* Added new make frame for selecting new frame after created.
* Used `hide-frame` instead of `delete-frame` while showing `lsp-ui-doc`, this
 should fixed while re-focus back to Emacs' frame issue while jumping away from Emacs.
* According to [company-lsp/131](https://github.com/tigersoldier/company-lsp/issues/131),
 revert `flx` support with `company-lsp`. Just set `company-lsp-cache-candidates`
 to `auto`.
* Removed package => `dimmer`.
* Installed new package => `lsp-ui`.
* Implemented `record` and `restore` windows status once utility functions.
* Installed new package `ivy-resize` manually.
* Added `define-it` config to change default output choice to `pop`.
* Installed new package `company-lsp`.
* ~~Implemented fuzzy match using `flx` with `company-lsp`.~~
* Applied `*lsp-` related buffer to diminish buffer list.
* Reverted the message wouldn't work the first time issue from Emacs 27.
* Added completed `lsp` configuration.
* Integrated better `lsp` signature buffer to dual window users/configurations.
* Organized to use `other-window` advice/hook instead of function wrapper.

## 6.0.2
> Released Dec 23, 2019

* Added window size change hook.
* Fixed after resized frame ivy window doesn't get resize issue.
* Added `license` template functionalities.
* Added `changelog` template functionalities.
* Added `typescript` template for [Cocos Creator](https://www.cocos.com/en/creator) scripting.
* Made `eldoc` compatible/interact with mouse click.
* Added peek frame util function.
* Fixed `feebleline` works with multiple frames.
* Added multiple cursors previous/next similar functions.
* Added simple make frame util function.
* Fixed `ivy` missing `ffap` dependency.
* Fixed message wouldn't work when `dashboard` is showing inside window.
* Added minibuffer first setup hook.
* Fixed `yascroll` wrong arguments error after Emacs version 27.
* Fixed `ivy` switch to buffer other window logic.
* Match enlarge/shrink windows times to `media` and `shell` windows.

## 6.0.1
> Released Dec 15, 2019

* Implemented new way to complete path using `ivy`.
* Implemented auto resize in `ivy` minibuffer window.
* Reverted `ivy` slash key for other usage; mainly due to it's own functionality is weird to me.
* Fixed `css-mode` weird face highlighting issue.
* Minor fixed for renaming inside minibuffer using `ivy`.
* Installed new package => `company-quickhelp-terminal` manually.

## 6.0.0
> Released Dec 10, 2019

* Switched from `helm` to `ivy` due to `helm`'s instability.
* Removed manually installed package => `reveal-in-folder`.
* Installed new package => `reveal-in-folder`.
* Installed new package => `parse-it`.
* Installed new package => `vs-light-theme` manually.
* Installed new package => `vs-dark-theme` manually.
* Starting from version `5.9.3`, theme work outside of this packages.
* Installed new package `file-header` manually.
* Configure package `file-header`.
* Fixed hex code color check function compatible to terminal.
* Added more commands to eldoc activation trigger list.
* Fixed Visual Studio dark theme color from line numbers.

## 5.9.2
> Released Dec 2, 2019

* Quick fixed for `helm` incompatible `completion-styles`.
* Revert `helm` incompatible `completion-styles` changes.
* Implemented set font util function.
* Minor fixed for iedit command when no kill ring.

## 5.9.0
> Released Nov 29, 2019

* Minor fixed for bury buffer function, it active only when `diminsh-buffer-mode` is on.
* Fixed media window logic.
* Fixed toggle shell window's logic.
* Fixed toggle video player window's logic.
* Make feebline compatible to video player.
* Fixed iedit `kill-ring` issue when enable/disable `iedit-mode`.
* Installed new package manually => `ffmpeg-player`.
* Implemented video player feature.
* Fixed isearch not showing with the first two characters.

## 5.8.9
> Released Nov 15, 2019

* Implemented switch `output`/`compilation` buffer keys.
* Make mode-line's color compatible to light theme.
* Changed default key bindings for search in project.
* Fixed `flycheck-pos-tip` will kill describe thing pos-tip functionalities.
* Installed new package => `flycheck-pos-tip`.
* Manually installed new package => `reveal-in-folder`.
* Fixed `multiple-cursors` lazy loading in navigate blank keys.
* Implemented `lsp-mode` to `goto-definition` function.
* Added quelpa upgrade process to standard upgrade process.
* Installed new package => `helm-describe-modes`.
* Fixed `multi-shell` maybe shell buffer logic.

## 5.8.8
> Released Nov 1, 2019

* Defined goto definition functions.
* Installed new package => `elisp-def`.
* Added default save all buffers function.
* Added reverse tab/untab save buffer function.
* Diminish buffer to `shell` and `eshell`.
* Added `multi-shell` config.
* Implemented multiple terminal functionalities.
* Manually installed new package => `multi-shell`.
* Unbind return from mc/keymap.
* Fixed default emmet expand line key, not doing any action by default.
* Implemented simplify safe jump to buffer window function.
* Changed no more jump to unwanted buffer.
* Installed new package => `request`.
* Fixed C-ret not doing anything with default.
* Installed new package => `wiki-summary`.

## 5.8.7
> Released Oct 22, 2019

* Removed useless hl keyword => `OR`.
* Instanlled new package => `visual-regexp`.
* Fixed key definition => `jcs-env.el`.
* Make tab and space compatible to VSCode's behaviour standards.
* Minor fixed with walk windows with multiple frames.
* Start supported language => `LESS`.
* Start supported language => `Kotlin`.
* Start supported language => `Dockerfile`.
* Set `smart-indent` as one option to move previous/next line.

## 5.8.6
> Released Oct 5, 2019

* Minor bug fixed - first window line pos inaccurate.
* Minor bug fixed - avoid enable/disable line number mode if not needed because is quite expensive.
* Bind backward/forward word capital to higher priority keys.
* Completely mimic VSCode `multiple-cursors` behaviour.
* Fixed `actionscript-mode`'s mutliline comment line up issue.
* Use `web-mode` instead of `vue-mode` for editing `.vue` file.
* Supported multiple compilation process.
* Enabled more autoload for `origami`.
* Installed new package => `vue-mode`.
* Minor fixed with commenting with openting `/*`.
* Fixed `python-mode` double quote key logic.
* Fixed remove end lines issue.
* Supported R programming language.
* Minor fixed for shell completion.
* Minor fixed for shell behaviour.
* Moved preferred settings to `prog-mode`.
* Implemented more `lsp-mode` to default `prog-mode`.
* Removed refresh font in `post-command-hook` functionality.
* Redefined color to => oop `tag`, `type`, `value` face.
* Fixed `highlight-indent-guides` execute `guide-region` multiple times when
using `jit-lock-register` function.
* Fixed `typescript-mode`'s highlighting.
* Fixed empty param issue list.

## 5.8.5
> Released Sep 24, 2019

* Implemented ask line endings to set coding system interactive util function.
* Added managed full test case for CI.
* Fixed log multiple times issue.
* Installed new package => `diminish-buffer`.
* Installed new package => `markdown-toc`.
* Installed new package => `browse-kill-ring`.
* Eliminate return with `void` type for typescript docstring.
* Fixed goto address not copy issue.
* Fixed mute apply with current message.
* Fixed buffer removal when reverting empty temporary file.
* Implemented `auto-highlight-symbol` with light/dark theme consideration.
* Redesign comment faces with light/dark theme consideration.
* Installed new package => `org-bullets`.

## 5.8.4
> Released Sep 16, 2019

* Fixed smart backspace/delete word key behaviour.
* Installed new package => `quelpa`.
* Fixed refresh buffer menu bug when switch buffer.
* Fixed shell mode key bindings.

## 5.8.3
> Released Sep 10, 2019

* Implemented `buffer-menu` filtering with `flx`.
* Implemented `mute-apply` util function.
* Removed package => `beacon`.
* Improved `feebleline` read-only config.
* Added read-only symbol to `feebleline` design.
* Fixed helm scrolling with window line height.
* Fixed keys `C-c` and `C-x`.

## 5.8.2
> Released Sep 3, 2019

* Removed package `helm-flx`.
* Installed new package => `helm-fuzzy`.
* Fixed some missing dependencies in some lazy loading functions.
* Fixed helm weird scrolling on the last selection issue.
* Rearrange key specify by mode.
* Clean up unused code from `jcs-buffer-menu.el` file.

## 5.8.1
> Released Aug 25, 2019

* Update buffer menu list when navigating through windows.
* Installed new package => `helm-flx`.
* Implemented horizontal center util function.
* Installed new package => `dap-mode`.
* Removed package => `sublimity`.
* Stopped support feature `smooth scroll`.
* Stopped support feature `minimap`.
* Installed new package => `lsp-mode`.
* Diminish `emmet-mode`.
* Diminish `company-fuzzy-mode`.

## 5.8.0
> Released Aug 17, 2019

* Complete key bindings document.
* Installed new package => `flx`.
* Installed new package => `company-fuzzy`.
* Minor tweak for `company` configuration for selection highlighting.
* Bind balance split window key as default split window behaviour.
* Minor tweak for `company` configuration.
* Make tab key compatible with `company`.
* Implemented ask csharp template functionalities to `csharp-mode`.
* Removed package => `company-statistics`.
* Minor changes for `company` package.
* Config eldoc trigger commands.
* Show tooltip even with one valid candidate in `company-mode`.
* Fixed display not ready issue on buffer menu.
* Fixed minor documentation issue.
* Supply `ruby-mode` and `rust-mode` indentation level's config.
* Implemented ability to record down the tab width across all major mode.
* Make tab width record to the next buffer with the same mode.
* Implemented non-verbose beginning/end of buffer functions.
* Bind non-verbose beginning/end of buffer key functions.
* Implemented increment/decrement tab size functions.
* Customize `feebleline` with system spaces or tabs displayed.
* Customize `feebleline` with tab size displayed.

## 5.7.0
> Released Jul 23, 2019

* Complete more preprocessor highlighting.
* Enabled `so-long-mode` as default.
* Implemented `buffer menu`'s return key.
* Implemented realtime updating buffer menu.
* Implemented filter functionality to buffer menu.
* Customize `snippet-mode` by adding `jcs-snippet-mode.el` file.
* Added snippet for `snippet-mode`.
* Fixed shell toggle logic, it no longer depends on the function state and now compatible to mutliple frames.
* Removed package => `indent-info`.
* Implemented remove carriage return symbol function.
* Fixed deletetion logic with tab width.
* Prevent loggin when refreshing dashboard, too verbose loggnig.
* Update `feebleline` customization.
* Installed new package => `feebleline`.
* Customize `feebleline` for default mode-line toggle.
* Implemented electric delete key.
* Implemented vs sharp key and bind to these following modes.
  - c-mode
  - c++-mode
  - csharp-mode
* Reverted package => `line-reminder`.
* Removed manually installed package => `line-indicators`.
* Set use linum when inside terminal for `line-reminder` package.
* Manually installed new package => `line-indicators`.
* Removed package => `line-reminder`.
* Installed new package => `centaur-tabs`.
* Installed new package => `company-statistics`.
* Removed package => `tabbar`.
* Fixed certain modes that does not apply `highlight-indent-guides` minor mode.
* Rename backward/forward capitcal word keys, much better naming.
* Fixed certain modes require error.
* Bind `package-list-packages` to `C-x C-p` instead of `C-p`.
* Fixed iedit-mode logic.
* Removed inconsistent key bindings for `c-mode` and `c++-mode`.
* Added unity snippets => `csharp-mode`.
* Install new package => `highlight-indent-guides`.
* Fixed `oop-func`'s built in docstring autoload.
* Installed new package => `alt-codes`.
* Implemented scratch other window function.
* Installed new package => `helm-file-preview`.
* Fixed multiple with-eval-after-load function.
* Implemented maybe kill `*scratch*` buffer function.
* Require `undo-tree` when needed.
* Try using first time post command startup.
* Implemented multiple with-eval-after-load function.

### 5.5.3
> Released Jun 25, 2019

* Implemeneted cheat sheet functions.
* Use regexp to ignore line numbers mode.
* Installed new mode => `gdscript-mode` for editing Godot Script file.
* Added `gdscript-mode` snippets using `yasnippets`.
* Had `helm-ag` requires pattern to `2`.
* Removed startup mode files, and moved their config to `jcs-mode.el` file.
   - `jcs-elisp-mode.el`
   - `jcs-lisp-mode.el`
   - `jcs-text-mode.el`
* Implemented insert header if buffer empty function for inserting file/mode header.
* Implemented html preview function.

### 5.5.1
> Released Jun 23, 2019

* Clean up code for better load speed.
* Fixed `helm-file-files` inserting `/` logic.
* Removed switch window by `M-0` to `M-9` keys.
* Implemented the display ascii-table function.
* Implemented remove item from `*dashboard*` buffer.
* Completed CI test.
* Update the `oop-func` logic, better and does not requires font lock implementation. Now it uses `search-string` instead.
* Upate text banner.
* Fixed dashboard next/prev blank line logic.
* Added text banner file => `./.emacs.jcs/banner/sink.txt`.
* Implemented autoloads functionalities to manually installed packages.

### 5.4.9
> Released Jun 19, 2019

* Implemented better dashboard buffer controlling util functions.
* Clean up customizes code section to => `~/.emacs.d/.jcs-custom.el` file.
* Removed package => `helm-gtags`.
* Installed new packae => `dumb-jump`.
* Use `dumb-jump` replacing `helm-gtags` functionalities.

### 5.4.7
> Released Jun 16, 2019

* Fixed focus in, refresh dashboard buffer hanging issue.
* Clean up log code.
* Use default helm display path option from `relative` to `root`.
* Installed new pacakge => `region-occurrences-highlighter`.
* Renamed `jcs-corresponding-file.el` to just `jcs-file.el`.
* Renamed `jcs-file-info-format.el` to `jcs-template.el`.
* Use find file in project instead of just find file for searching corresponding file functionalities.
* Update `*dashboard*` buffer when access recent projects list.
* Removed some of useless `require`s.
* Removed some of useless plugin's config.
* Ready the configuration for Emacs version 27.
* Disable `multiple-cursors` when navgiating blank line.
* Installed new pacakge => `yascroll`.
* Added customize `yascroll` face by theme color function.

### 5.4.5
> Released Jun 8, 2019

* Fixed `helm-projectile` return key not exiting minibuffer issue.
* Re-implements `helm-files` related functions. For find files other windows.
* Clean up some compile warningins.
* Optimized configuration down to startup time around from `2` to `6` seconds.
* Optimized configuration down to startup time around from `4` to `8` seconds.
* Added more `helm` find files keymap to match OS's file explorer's navigation system.

### 5.4.3
> Released Jun 2, 2019

* Installed new pacakge => `esup`.
* Clean package initialization using `require` keyword.

### 5.4.1
> Released May 30, 2019

* Fixed `50%` of config compile issues.
* Enable compile version of this config.
* Fixed `helm` theme inconsistent to the `vs-light` theme.
* Fixed `right-click-context` package's. ([#2](../../pull/2) and [#7](../../pull/7))
* Removed package `pdf-tools`.
* Implemented automatically enable `read-only-mode` when view source or library files.
* Removed package `floobits`.
* Added ignore activating line numbers by major mode list.
* Updated line numbers ignore buffer list.
* Remove before/after init files.
* Optimized more plugins to `jcs-plugin.el` file.
* Fixed `compilation-mode-hook` from `jcs-env.el` file.

### 5.3.9
> Released May 20, 2019

* Reduced duplicated code in `jcs-comment.el` file.
* Removed manually installed package => `show-eol`.
* Installed new pacakge => `show-eol`.
* Make `comment` and `uncomment` related functions compatbile to `line-reminder` package.
* Added is behind last char at line util function.
* Added `point` option to infront first char at line util function.
* Added `*Package-Lint*` to line numbers not displayed list.
* Manually updated `show-eol` package manually => `20190517.001`.
* Start supports `dart` by using `dart-mode`.
* Start supports `pascal` by using `pascal-mode`.
* Start supports `Object Pascal`/`Delphi` by using `opascal-mode`.
* Added `dart-mode`'s snippets.
* Added `pascal-mode`'s snippets.
* Added `opascal-mode`'s snippets.
* Manually updated `show-eol` package manually => `20190513.002`.
* Manually updated `show-eol` package manually => `20190513.001`.
* Implements self defined comment or string util function.
* Remove `jcs-top-level-active` global `defvar` for keyboard quit check.
* Remove `jcs-minibuffer-active` global `defvar` for minibuffer active check.
* Fixed `hl-todo-mode` not working in `web-mode` by redefine highlighting condition => `jcs-plugin.el` file.
* Implements calc eval region function for calculating the region and replace it with the calculated result.
* Implements backward/forward symbol functions for interactive use.
* Revert `haxe-mode` so it works for now, but still leave with no maintainer with this mode.
* Implements `get window` and `get window id` util functions.
* Fixed reset dashboard banner not refresh issue.
* Installed new pacakge => `hl-todo`.
* Implements check if light or dark color util functions.
* Added default light theme.
* Manually installed package `show-eol`.
* Make `text-mode` to the top for ready to override by other mode.
* Organized configuration's directory structure.
* Remove `jcs-font.el` file and put the `font` config to the `jcs-env.el` and `jcs-plugin.el` files.
* Update dependency list.
* Added deactive all line numbers modes util function.
* Fixed toggle mode line key binding.
* Fixed active line numbers by mode logic, we use to deactive the line numbers mode for now instead of just ignore it.
* Fixed modes not activated after revert issue.
* Implements `toggle-mode-line`.
* Cleanup `web-mode`'s util functions.
* Unbind `web-mode` util functions from `jcs-web-func.el` file.
* Minor fixed with some typo.

### 5.3.7
> Released Apr 22, 2019

* Installed new pacakge => `goto-char-preview`.
* Added new snippet for `react.js` in html.
* Added new snippet for `bootstrap` in html.
* Added new snippet for `three.js` in html.
* If region active, when `isearch` is activated we use region instead.
* Fixed `css-mode` return key.
* Fixed css number not highlighting correctly.
* Installed new pacakge => `isearch-project`.
* Bind `isearch-project-forward` to implement `cross-mode` search through project ability.
* Implements helm projectile find file other window function.
* Split electric pair pairs to each specific mode.
* Remove `shift-select` package, the package is still remained unstable.
* Sort keys in alphabetic order category.
* Revert `shift-select` package => version `20190419.001`.
* Implements is symbol contain in list of symbol util function.
* Added more key bindings for switching windows.
* Added remove trailing lines at the end of buffer util function.
* Implements self design mark whole buffer.
* Remove README, LICENSE, bochsrc files default to `org-mode`.
* Added `html-mode` and `js-mode` snippets.
* Manually installed `shift-select` package.
* Added more `:defer` to more packages.
* Removed many unused packages.
* Fixed `jcs-flycheck-mode` logic.
* Remove smart shift select home/end functions.
* Complete set of manual install package section.

### 5.3.5
> Released Apr 15, 2019

* Implements selecting windows by using windows' index.
* Removed `elpy` package.
* Removed `find-file-in-project` package.
* Removed `ivy` package.
* Installed `projectile` package.
* Removed `js2-refactor` package.
* Implements `multiple-cursors` quick hand functions.
* Fixed vs curly bracket logic.
* Start supports `elixir` by using `elixir-mode`.
* Start supports `erlang` by using `erlang-mode`.
* Installed `helm-projectile` package.
* Installed new pacakge => `buffer-move`.
* Fixed `same file other window` bug.
* Fixed `undo-tree` occurs error when trying to kill its parent buffer.
* Starts featuers documentation under `./features/` folder.
* Split `.ini` and `.properties` mode.
* Added `jcs-properties-mode.el` for supporting java properties file.

### 5.3.3
> Released Apr 12, 2019

* Installed new pacakge => `dashboard`.
* Installed new pacakge => `beacon`.
* Minor fixed from version `5.3.2`.

### 5.3.2
> Released Apr 11, 2019

* Added `gitconfig` configurations.
* Use `with-eval-after-load` macro to speed up startup time.
* Huge update on the startup time, now the average startup time is lower than `10` seconds.
* Customize `company`'s appearance close to `auto-complete`'s appearance.
* Added config to make `company` a bit more close to `auto-complete`'s behavior.
* Added `show hover` function related to VSCode `Show Hover` key.

### 5.3.1
> Released Apr 9, 2019

* Kill `undo-tree-visualizer` when killing undoing buffer.
* Start adding own snippets using `yasnippet`.
* Rename all `cs` related naming to `csharp` for consistency.
* Rename all `elisp` related naming to `emacs-lisp` for consistency.
* Rename all `cbl` related naming to `cobol` for consistency.
* Split `cmake-mode` and `makefile-mode` into two files.
* Installed new pacakge => `company-quickhelp`.
* Remove `auto-complete` and use `company` instead.
* Start supports GLSL file
* Removed manually installed `verilog-mode`, it mode is already merged into GNU Emacs.
* `polymode` package added by system.
* Don't use `narrow-to-region`, instead we just pass in the `start` point and `end` point.
* Installed new pacakge => `origami`.
* Use `origami` as default folding system to this config.
* Implements `jcs-message-func.el` file.
  -> Erase *Messages* buffer.
  -> Erase *Messages* buffer without closing it.
* Make oop docstring compatible with ref and pointer in c and c++ mode.
* Fixed kill buffer after exit buffer menu mode.
* Fully implements TypeScript docstring.
* Implements ActionScript docstirng.
* Implements `web-mode`'s version front curly bracket key and bind it to web-mode.
* Fixed docstring display issue in `web-mode`'s php file.
* Fixed vs curly bracket logic.
* Added optional to scroll up/down line functions.
* Complete line related util functions.
* Remove `Alex Shinn`'s `css-mode`, use Emacs's default `css-mode` instead.
* Added larger window height check util funtion.
* Implements ActionScript docstring entry point.
* Implements CSharp docstring entry point.
* Fixed only one file opened, switch to default Emacs buffer issue.
* Installed new pacakge => `yasnippet-snippets`.
* Added configuration for `yasnippet` and `yasnippet-snippets`.
* Bind electric backspace key to certain modes as default key binding.
* Improved undo/redo keys performance when using `undo-tree`.
* Simplify code in `jcs-oop.el` file.

### 5.2.9
> Released Mar 28, 2019

* Implements `typescript-mode` docstring.
* Added `Startup Time` section in the `README.md` file for describing the
current condition for using this configuration when starting up Emacs.
* Added advice to `save-buffer` key to disable `undo-tree`.
* Fixed compile target script, wrong param name.
* Move `jcs-helm.el` functions to `jcs-helm-func.el` file and delete `jcs-helm.el` file.
* Manually update `reload-emacs` package => 20190326.001.
* Added first visible line pos util functions.
* Make revert window state to reopn this buffer key.
* Fixed reopen this buffer key, make compatible with opening the same buffer in different/mutliple windows.
* Removed global linum mode when using undo-tree.
* Implemented reopen this buffer key.
* Added line number related functions => `jcs-function.el` file.
* Added mixed of using `display-line-numbers-mode` and `linum-mode`, for any
file that uses `line-reminder` mode use `linum-mode`.  Other we use `display-line-numbers-mode`.
* Fixed `overwrite-mode` cursor not working.
* Added walk through each window util function.
* Bind reload emacs and restart emacs.
* Remove self design reload emacs function.
* Manually install package => `reload-emacs`.
* Config `reload-emacs` package using `use-package` in => `jcs-plugin.el` file.
* Rename plugin advice function name for accuracy purpose => `jcs-plugin.el` file.
* Bind isearch forward at point key.
* Remove search forward/backward at point functions.
* Installed new pacakge => `move-text`.
* Bind rebind keys after define `jcs-global-key-rebind` function.
* Installed new pacakge => `restart-emacs`.
* Retain reload emacs functionalities.
* Fixed smart indent up/down keys in `css-mode`.
* Remove unused packages.
  - auto-complete-c-headers
  - google-c-style
* Speedup Emacs startup time.
* Move erase buffer to somewhere more reasonable.
* Use require instead of load path.
* Fixed check `truncate-lines`, this isn't minor-mode is actually a variable with t or nil.

### 5.2.5
> Released Mar 11, 2019

* Installed new pacakge => `indicators`.
* Implements toggle transparency window that will record dowwn
 the last transparent alpha level. This feature polished the
 user experience wise.
* Implements switch window group layout vertically/horizontally key's functionality.
* Installed new pacakge => `focus`.
* Installed new package => `dimmer`.
* Fixed speedbar not starting in the correct directory tree using `default-directory`
variable instead of fiddle method of fixing this issue.
* Manage most plugin configurations using `use-package` package.
* Revert part of the code, fixed indentation incorrect when doing docstring comment style.

### 5.2.4
> Released Mar 6, 2019

* Added screen config section => `jcs-env.el` file.
* Added goto-line-preview section and configurations.
* Start using `use-package` in the config, add `Package Management` section to the feature list.
* Fixed speedbar not opening the current file directory issue.
* Rebind some key bindings for more reasonable reason, see `./doc/keybindings.txt` file.
* Diminish minor modes, `overwrite-mode` and `eldoc-mode`.
* Make toggle terminal command compatible to vscode preset's key bindings.
* Bind `describe-bindings` key to `C-k C-s`, compatible to vscode preset's key bindings.
* Upgrade with more math functions => `jcs-math.el` file.
* Rebind toggle cross/depends mode key to `C-~` key.
* Fixed transparent window util functions and reduced duplicate code.
* Rebind text scalle up/down key to `C-=` and `C--`.
* Rebind transparent frame increament/decreament key to `M-=` and `M--`.
* Update some key bindings to `./doc/keybindings.txt` file.
* Added typescript docstring configurations.
* Added `.properties` extension to default as `ini-mode`.
* Fixed css and web return key => `jcs-web-func.el` file.
* No longer needed resolve `goto-line-preview-goto-line` that does not go back
to original position issue, the package resolved itself.
* Reserve `goto-line-preview` config section.
* Update key command from => `goto-line-preview-goto-line` to `goto-line-preview`.
* Installed new package => `goto-line-preview`.
* Remove `goto-line` key, instead we use package `goto-line-preview` from melpa.
* Make compatible with old `jcs-goto-line` key, by having check in `jcs-hook.el` file.
* Reserve minibuffer post command hook.
* Added `goto-lnie-preview` config section in the `jcs-plugin.el` file.
* Added top level activation flag.
* Move minibuffer hook to hook file.
* Added improved goto line navigation functionalities and bind to original `goto-line` key.
* Set keys compatible to VS Code default key bindnigs.
* Fixed toggle vertical/horizontal editor layout functionality that does not works
on the second window in the current frame. Notice this is only a temporary fixed.
* Fixed magit installation error by updating its' dependencies.
* Update beginning/end of visual line the same behaviours as the VSCode text editor's key behaviours.
* Make ALT-z toggle `truncate-line-mode`, so it compatible to VSCode's key presets.
* Added jcs home and end keys functionalities.
* Bind home and end keys functions.
* Move jcs web mode truncate line functionality to hook instead of locate every key functions.
* Remove web left/right key functions/functionalities.
* Avoid auto truncate line functionalities while navigating empty lines in web-mode.
* Revert jcs set init face.
* Load set init face in js2-mode.

### 5.2.2
> Released Feb 6, 2019

* Manually update manual packages.
* Fixed readme description.
* Added key bindings description to readme file.
* Update project description and elaborates more about it.
* Implements symbol util functions.
* Remove single line comments font lock keywords => mapc.
* Added jcs python docstring face.
* Fixed python tab key binding with weird action.
* Ensure python tab width is 4 instead of default of 8.
* Remove load todo, load log and insert-timeofday command functions.
* Use defense programming in current char string util function.
* Added is-killed returned value to jcs-maybe-kill-current-buffer util function.
* Fixed jcs' count window util function.
* Fixed re-builder's maybe kill this buffer function using is-killed variable.
* Implements python return function => jcs-python-func.el.
* Organize legacy code => jcs-python-func.el.
* Fixed python insert docstring function, for second situation, between two double quotes.

### 5.2.1
> Released Jan 2, 2019

* Remove history, is no longer needed.
* Added load face order, and just reload instead of operate the list functions.
* Added sharp single line comment face.
* Compatible to electric pair command in python mode.
* Fixed move forward/backward word navigation util functions.
* Added ask python template and use it when creating new python file.
* Added python plain and class template.
* Mark version 5.2.1 and release one version.
* Revert maybe kill this buffer function and add ecp-same arg.
* Update definition for maybe kill this buffer function => jcs-edit.el.
* Optimize switch to prev/next buffer util functions.
* Diminish right click context mode.
* Some modifications for maybe kill buffer key.
* Added next/prev buffer util functions.
* Added print buffer util functions.
* Install new package => right-click-context.
* Added package to pre-install package list => right-click-context.
* Enable right-click-context as default in plugin config file => jcs-plugin.el.
* Start support INI file, customize the `ini-mode' with jcs-ini-mode.el file.
* Fixed coding style => jcs-file-info-format.el.
* Added electric backspace util function.
* Added electric open/close pair related functions.
* Fixed verbose char to byte and char to string util functions.
* Added yaml func file for yaml mode functions => jcs-yaml-func.el.
* Added new package 'auto-rename-tag' to preinstall package list.
* Added new package 'htmltagwrap' to preinstall package list.
* Diminish the 'auto-rename-tag' minor mode.
* Active diminish by requiring the package you want to diminish => `auto-rename-tag`, bug fixed.
* Added hex and char section to last-command-event doc => doc/last-command-event.txt.
* Added doc/last-command-event.txt for record all the last-command-event's returns
 value.
* Added indent-info package and it's config.
* Fixed insert header only when buffer-file-name variable available.
* Fixed bug by adding percise check => jcs-maybe-kill-current-buffer function
 in jcs-edit.el file.
* Implements check how many times the same buffer shown in different windows
 => jcs-buffer-showns function in jcs-window.el file.

### 5.1.9
> Released Dec 3, 2018

* Bug fixed, make percise return key for web-mode => jcs-web-return-key.
* Added gitattribute custom mode hook.
* Make one history => ### 2018-11-25.
* Implemented YAML mode hook, => jcs-yaml-mode.el file.
* Start support Swift file, customize the `swift-mode` with jcs-swift-mode.el file.
* Start support Rust file, customize the `rust-mode` with jcs-rust-mode.el file.
* Start support Ruby file, customize the `ruby-mode` with jcs-ruby-mode.el file.
* Fixed web-mode highlighting missing when apply ASP.NET Razor v3 comment highlighting rule.
* Added `jcs-post-command-hook` in `jcs-hook.el` in order to fix highlihging
missing when editing file using web-mode.
* Start support YAML file, install major mode `yaml-mode`.
* Implement web return key functionalities.
* Increase readabilities for util module.
* Start support Markdown file, install major mode markdown-mode`.
* Added customize markdown mode configurations.
* Completey remove neotree.
* Use `speedbar` and `sr-speedbar` instead of `neotree`.
* Implemented `speedbar` and `sr-speedbar` customize functions.
* Implemented `nhexl-mode' configurations.
* Long-overdue support language `Verilog', starting from now on support this language.
* Implement deleting between functionailities, and add some custom function for certain generally use symbol in programming.
* Fixed check current character occurs error issue at point of beginning of the buffer.
* Rename template to be more specific and precise on the naming.
* Added Lisp header template.
* Fixed weird insert header file format's function description in each mode file.

### 5.1.7
> Released Oct 11, 2018

* Start support TypeScript file, install major mode typescript-mode.
* Added typescript header format template.
* Make `jayces-mode` to package.
* Added `tabbar` package and set the env settings/key bindings.
* Added `javadoc-lookup` package and set the env settings/key bindings.
* Start support Clojure, ClojureScript and Clojure Source file, install major mode `clojure-mode`.
* Update license and prorject version to 5.1.7.
* Improve enable/disable truncate lines mode.
* Remove web return key, seems like we no longer need this key function anymore.
* Rename, remove emacs prefix to all doc.
* Added Emacs' syntax table document.
* Added Emacs' regular expression document.
* Added recentf-file mode environment settings => jcs-env.el.
* Bind open recent files key => jcs-global-key.el.
* Update key binding note => open recent files key.
* Implemented jcs-emmet-expand-line wrapper in order to fix on link goto address issue.
* Bind the key in emmet mode keymap.
* Added jcs-count-frames function for multiple window's frame count.
* Fixed maybe kill this buffer function with the same file name but different directory issue.
* Added more face to fixme mode list.
* Move face settings to jcs-face.el.
* Load fixedme face after all initialize, so we cover all the faces.
* Change the `default-directory' variable when compiling a script to the directory the current script is currently at.

### 5.1.5
> Released Sep 26, 2018

* Implement the following three util functions..
   => jcs-current-whitespace-p
   => jcs-current-tab-p
   => jcs-current-whitespace-or-tab-p
* Implement jcs-text-scale-increase and jcs-text-scale-decrease
 function in order to fix the `line-reminder` plugin issue.
* Fix everytime it search forward recursive, it will centerl the window issue.
But does not happens in search backward recursive... Weird! => locate in
`jcs-nav.el` file.
* Update license and prorject version to 5.1.5.
* Added example package files for future package example and installation location standard.
* Implemented VS like cut key in jcs-vs-func.el file.
* Bind the vs like cut key as default cuty key in global mode.
* Added search forward/backward colon/semicolon/greater and less than sign in jcs-nav.el module.
* Rename function check first forward/backward character with in line post-fix.
* Added check fist forward/backward character to limit to the whole buffer.
* VS like function implemented => jcs-vs-func.el file.
* Load vs like functions to each related mode.
* Remove vs like function key binding as global key, instead we declare it inside
specific mode that needed to have vs like function key bindings in it.
* Rename next/previous blank line function with jcs prefix.
* Fixed haxe-mode cannot switch frame issue.
* Added quote symbol to specify the correct extension to the correct major mode.
* Start support Haxe file, install major mode 'haxe-mode'.
* jcs-haxe-mode for own control of editing Haxe file.
* Added haxe_template.txt for Haxe file's header.
* Package dependencis changes through melpa package manager updates.
* Package dependecies list changes while update packages on melpa.
* Rearrange package dependencies package list.

### 5.1.3
> Released Jul 20, 2018

* Added 'use-package' package to pre-install package list.
* Update license and prorject version to 5.1.3.
* Added json-mode package to package dependency list.
* 'wgrep' package added back to install list, know idea why it seems like get reject by Emacs. Anyway, is back on Emacs again.
* Install new package 'project-abbrev', and remove manually install code for this package.
* Added jcs-ex-pkg example package, for future self package development.
* Remove manually install 'line-reminder' package, install it on melpa. The package 'line-reminder' is currently on melpa.
* Change package name from 'custom-abbrev' to 'project-abbrev'.
* Added double dash comment font lock face for mode it uses '--' to do single line comment.
* Added java save functionalities/functions work with `organize-imports-java` package, when first time save reload local source paths.
* Fixed get current point face name for Emcas version 26.
* Use util jcs- prefix check current face function instead of same code everywhere.
* Added `null` and `void` face to modifier face.
* Added haskell to support language list -> README.md.
* Added haskell mode .el file.
* Added Haskell template.
* Added math module for future math use.
* Remove trans window module to just window module.
* Bind message buffer keymap with earse message buffer.
* Load math module and remove load trans window module.
* Simplify trans window module's code.
* Added print timestamps with multiple version function/functionality.
* Rebind 're-builder' key and 'Rename current buffer/filename' key.
* Remove timestamp version 3 properties and it function.
* Update project version to 5.1.1.
* Re-arrange readme file to sort support languages by alphabetic order.
* Bind save buffer key with set file coding system functionality in `sh-mode`.

### 5.1.0
> Released Jun 16, 2018

* Added # to all interactive function operative and add new key binding toggle enlarge window selected key.
* Added few balance window functions and enlarget current selected window function.
* Added set all local variable function. Pass in as symbol.
* Rename duplicate line function with prefex 'jcs-' infront.
* Added enlarge current selected window key binding doc.
* Added overwride mode rewrapper function functionality.
* Force maximize frame after reload Emacs and remove helm function module.
* Rename jcs-new-window to jcs-new-frame for better naming and understanding.
* Separate helm function to individual helm-func file.
* Added frame func file/module.
* Make one history => ### 2018-06-16.
* Added package-autoremove key binding note to project doc.
* Replace `blank-mode` pacakge to `whitespace` package, is built-in now.
* Added certain more keyword to highlight for programming usage, check on `jcs-env.el` file.
* Added jcs-compile function rewrapper functionality.
* Remove 'blank-mode' from pre-install package list.
* Update project version to 5.1.0.
* Fixed normal web comment highlighting.
* Better way of checking if beginning of line using 'current-column' function.
* Added toggle read-only mode key binding and make note to emacs key bindings doc.
* Revert back to error handling with custom-abbrev expansion key.
* Added is default face functionality to utility module.
* Update emacs version record to Emacs 26.1.
* Update project compatible with Emacs version 26.1.
* Make one history for Emacs version 26.1 => ### 2018-06-11.
* Update project version to 5.0.5.
* Fixed error handle still going url after custom expansion with key bindings `ctrl + return`.
* Rename function from duplicate-line to jcs-duplicate-line for consistency.
* Manually upgrade pacakge `use-ttf` to version 20180609.
* Remove casey text mode hook.
* Error handling decision on not finding the version control root directory.
* Added find file in project and current directory, also a design decision.
 Plus add the `jcs-find-file-in-project-and-current-dir` and
`jcs-select-find-file-current-dir` functions. Fixed the bug
for `jcs-select-find-file-in-project' function.
* Create 'jcs-dev.el' file for development related functions file put here.
* Remake open-todo, open-update-log, makescript/runscript without asking.
* Upgrade package 'line-reminder' package manually => 20180603.
* Manually install package 'custom-abbrev'.
* Implement `jcs-ctrl-return-key' functionality for JayCeS default control return key. It uses priority function list to handle each requirement.
* Update keybindings doc describe ctrl-return key.
* Remove manually isntall 'com-css-sort' package, use melpa package manager instead.
* Diminish line-reminder pacakge.
* Update project version to 5.0.1.
* Upgrade package 'line-reminder' package manually => 20180601.
* Upgrade package 'line-reminder' package manually => 20180531.
* Manually install 'line-reminder' package => 20180529.
* Remove none needed autoload prefix function from jcs-util file.
* Set global line reminder mode enable as default.
* Added redo key to 'org-mode'.
* Wrong according key bindings, place it by categories.
* Added triple char style comment prefix check functionalities.
* Fixed Lua comment active docstring error.
* Fixed Visual CSharp comment active docstring error.
* Added prefix message and value delimiter arguments for `jcs-log-list' function.
* Manually update 'use-ttf' package to 20180526.
* Package 'organize-imports-java' is on melpa, no longer need to manually install the package.

### 5.0.0
> Released May 25, 2018

* Update 'use-ttf' package manually to 20180525.
* Trasnfer data from `.emacs.d' to `.emacs.jcs'.
* Make one history => ### 2018-05-25.
* Future history for first version of .emacs.jcs directory tree view/template.
* Update project version to 5.0.0, huge data transfer/rename from '.emacs.d' folder to '.emacs.jcs' folder.
* Update `use-ttf` package manually to 20180523.
* Added first version of fonts.
* Make shown prefex for better function readability for certain function.
 Like `jcs-jump-to-shown-buffer` instead just `jcs-jump-to-buffer`.
* New manage file, jcs-font.el file.
* Split `jcs-font.el` module to individual package => `use-ttf`.
* Manually install package => `use-ttf`.
* Use cl-lib instead of my own ugly method implementation.
* Jump to *Messages* window after do the logging/message.
* Use undo tree with the better performance without opening/closing the undo-tree-visualizer mode all the time.
* Added jump to buffer functionality
* Goto *Messages* buffer and end of buffer when do jcs type of logging functions.
* Fixed Visual Studio's C# type of commenting method. Weird action when having
two slashes, does not detect the Visual Studio's type of prefix comment symbol
pretty well.
* Added `all-the-icons' package to preinstall package list.
* Close *undo-tree* buffer when save.
* Remove sorting CSS attributes before save, I think is never been useful.
* Manually update packages 'com-css-sort' and 'organize-imports-java'.
* Added shell key up and key down functionalities.
* Remap shell-mode key bindings for trying simulate the real shell action.
* Added shell command completion functionality/function and map it in the shell-mode.
* Fixed css-mode comment font face does not work with dash.
* Bind upgrade package to package mode.
* Fixed depends mode not active before we enter at least once of the minibuffer.
* Added completion key binding to shell-mode.
* Added deletetion series of functionalities and key bindgins.
* Added start/last line of buffer boolean check functionalities for future utility use.
* Improve reload emacs functionality.
* Better default with shell mode control, more like the normal/general/ common terminal prompt.
* Added undo tree keymap comment or uncomment overwirte.
* Added save excursion for revert all buffer.
* Added toggle undo tree related functions.
* Added package upgrade all function.
* Make one history => ### 2018-05-15, upgrade all package to the newest version.
* Added shell backspace functionality.
* Added undo/redo key bindings with `undo-tree`.
* Added `jcs-shell-mode.el` file for shell mode managing.

### 4.8.1
> Released May 12, 2018

* Complete all naming convention to `jcs`.
* Make one history => ### 2018-05-11.
* Make comment methods exactly the same as Visual Studio IDE in csharp mode.
* Added header smart indent down and up for csharp mode in `jcs-cs-func.el` file.
* Split file into `jcs-hook.el` file from `jcs-after-init.el` file.
* Update project version to 4.8.1.
* Fixed csharp distinguish between docstring and normal comment line.
* Set default f7 key bindings to find file other window.
* Organize corresponding functions in c/c++ mode to function file.
* Manually install update `organize-imports-java' package.
* Added first character forward/backward check functionalaities.
* Use first character forward/backward check instead of string-match to the end of line and beginning of line.
* Fixed shell not prompt the first time issue with error handling.
* Added remove trailing whitespace current line functionality.
* Change all files header to classic JayCeS header.
* Set default major mode to org-mode.
* Added comment regexp for web-mode, compatible with ASP.NET project.
* Added check current char string match functionality.
* Auto truncate lines in web mode, add `default' face check twice. Make default
check on the first character.
* Make kill whole line function goto the same column as current line.
* Added web-mode rewrapper function for future handy use.

### 4.7.4
> Released May 4, 2018

* Check end of line trigger enable/disable auto truncate lines effect.
* Remove unuse readme file -> README.txt.
* Update project version to 4.7.4.
* Implement auto truncate lines functionalties.
* Enable auto truncate lines functions in web mode as default functionalities.
* Added get current line number functionalities.
* Update `organize-imports-java' package manually.
* Install com-css-sort package manually.
* Added error handling for `helm-do-ag-this-file` command. No need to switch
to cross mode manually anymore. It will just switch to cross mode automatically.
The implementation can be found in `jcs-helm-do-ag-this-file'` function.
* Fixed some autoload/interactive functions.
* Fixed failed search on move to next/previous blank line functions.
*  Fixed CSS type face highlighting.
* Change indentation and change mode to from if statement to cond statement.
* Added toggle web mode offsetless elements.
* Set web mode offsetless element variable.
* Key bindings comment detail update.
* Change `html' template's head and body tag to the same level as html tag.
* Added default ASP .NET's extension to web-mode.
* Added manually install section in .emacs file.
* History removal, keep one history is enough.
* Revert history files.
* Install organize-imports-java' package manully.
* Added plugin organize-imports-java' solve Java imports functionaltity.
* Temporary install `organize-imports-java' package manully, ready for melpa to publish it.
* Added some functionalities for modefiying list object.
* Reformat some parameters in order to match flycheck standard.
* Check default directory for java packaging path including functionalities.
* Fixed comment file path match flycheck standard.
* Added search version control directory and it related functionalities.
* Fixed c++ mode namespace font lock face and compatible with c mode.
* Added java package declaration functionality.
* Added empty line between two lines of code functionalities.

### 4.7.1
> Released Apr 4, 2018

* Added `which-key` package to pre-install list.
* Added `which-key` config.
* Update project version to 4.7.1.
* Added doc string properties file.
* Apply hot reloading to doc string properties customization.
* Added processing mode config, starting support processing.
* Added processing template file.
* Rename save buffer to proper naming with tabify and untabify.
* Added save tabify to global keymap.
* Added spaces and tabs regexp for editing preprocessor type programming langauges.
* Added tabs to all spaces regexp.
* Added change font functionalities and section.
* Added change font key binding and update key bindings note/doc.
* Added BASIC template.
* Added BASIC mode.
* Fixed :graph: regexp to limited version -> a-zA-Z0-9.
* Old manage file info function call fixed to current version.
* Make compatbile with c regexp missing fixed.
* Cancel using c++ for .c file, back to c-mode.
* Clean check all templates and remove template function's keywrods.
* Make all modes compatible with template hot reloading system.
* Added `jcs-perl-mode.el' file for Perl script editing.
* Added after init hook for preload template for half hot reloading.
* Added first version of all supported language.
* Added missing language to language support list.
* Added scala mode for scale file editing.
* Remove coding template insertion, add template 3rd hot reloading system.
* Added prompt instead of text questioning in minibuffer for template questioning function.
* First add templates file for general coding languages.
* Added ini file for keyword variables in template.
* Added make file tempate for python and java, identify those in cc too.
* Update version to 4.6.0.
* Added org table navigation key bindgs to org mode.
* Added org table navigation functionalities.
* Use blank and graph regexp instead of guessing all needed characters for comment highlighting in org-mode.
* Added `bochsrc' as default extension in org-mode, meaning use org-mode when editing bochsrc file.
* Added `jcs-tab-key' function for global tab key command and key bindings.
* Insert spaces by tab width function implemented and located in util module.
* Added jcs-log-list functionaly for debug usage.
* Fixed c/c++ mode define oop doc string array list reverse order issue with built-in lisp's `revers' function.
* Fixed comment closing c style comment block and organize elisp for better usage.
* Added `.` for oop value's regexp.
* Make special key's regexp compatible without space.
* Update project version to 4.5.6.
* Make align function repeat as default.
* Remove special key `' key bindings because we use built-int electric pair system instead.
* Added txt func file for editing jayces own use text file.
* Added delete space infront of line functionalities.
* Added smart indent up/down for web-mode.
* Added web return key functionality.
* Set default org mode no fold.
* Added align comment functionalities.
* Deactive mark to unselect current region if align region not active.
* Added ignore-errors for helm find gtags in order to restore previous cross/local mode.
* Fixed nasm comment key forward and spacing issue.
* Fixed nasm return key at the end of buffer can not make new line issue.
* Align repeat functionality implemented.
* Added `!` and `.` to special keyword regexp.
* Added special key electric functionaltity.
* Added special key key binding to global keymap.
  - Here special key mean any word between/inside \` and ' is a special keyword.
* Make sure back to original mode (cross/depend) once exit gtags's mini-buffer.
* Use default nasm comment function before do our own functionalities in `jcs-nasm-comment` function.
* Fixed python and lua mode regexp problem for doc-string indentifier.
* Make oop doc string support multiple line coding style.
* Added current word equal functionality.
* Added move forward/backward to word functionalities.
* Added support python multi-lines coding style.
* Refactor env file for better readability, majorly use defacce instead of make-face.
* Apply option comment doc-string option choice to the function.
* Added ` between ' special key word highlighting.
* Added highlight restriction to value regexp.
* Added nasm return and comment key.
* Added doc stirng check before insert docstring.
* Added goto first char of line functonalities for reduce duplicate code.
* Ignore all tabs and spaces after preproc keywords.
* Added logging/debug functionalities.
* Added convert tab to space and space to tab functionalities.
* Added real backspace and real space functionalities.
* Get notice about Ctrl-r key bindings will be rebind everytime, fix backspace key bindings issue.
* Added default link command to both application/library makefile templates.
* Fixed wrong spelling for diasm command in makefile template.
* Change jcs-delete-forward/backward-current-char-repeat's key bindings, to avoid bind key with backspace.
* Make sure delete region before delete line for kill whole line command.
* Complete .asm .s .S support assembly makefile template.
* Space check for compatibility py backspace command to tab.
* Refactor util with flycheck checker.
* Added python like space keybindings to nasm mode.
* Modefied key bindings compatible with search, captial navigation and kill navigation.
* Added nasm func file for nasm mode functionalities.
* Refactor jcs-edit file to jcs-buffer-menu file.
* Make percise to shell toggle function.
* Added find file hook.
* Added buffer menu functions.
* Make history/backup.
* Added isearch at point forward and backward functionalities.
* Enable auto complete mode in nasm mode as default.
* Change CMake bindings work like Python mode key bindings or functionalities.
* Added 'functions' for makefile info.
* Make compatible with .S file extension.
* Added `mount` and `buildimg` commands for makefile template.
* Added enum doc string for c and c++ mode.
* Added linux and macosx build env and header file list.
* Added assembly language commands and flags for makefile file formant.
* Added '@' symbol for normal bash commands.
* Added emacs keybindings note with open update log and todo list.
* Move move empty line up and down to nav file.
* Added open update log and open todo list functionalities.
* Added 'jcs-preproc-fun.el' file.
* Make compatible define docstring with preproc face type.
* Refactor using when instead of if statement.
* Added oop init set face function.
* Added local face modefication like visual studio type style.
* Customize cmake division comment.
* Refactor using 'when' instead of 'if' statement.
* Change class comment type in Python mode.
* Added clean a lib and so lib command to 'realclean' command.
* Place casey's global key bindings to jcs global key binding file.
* Added c-flags to build dynamic library command.
* Added package filter.
* Added 'all' option for package filter.
* Added web-mode and php-mode doc string.
* Makefile format work with static lib and dynamic lib.
* End with keyword with non nil keyword at third param, fix font lock not working issue.
* Added goto start of the comment and goto end of the comment functionalities.
* Properly name default static and dynamic lib default build name.
* Fixed CMake not applying issue.
* Fixed makefile format. static to .a, dynamic to .so, $< to $^ sign.
* Added css highlighting.
* Added makefile templates.
* Added cmake/mke file ask template functionalities.
* Loaded txt mode for later since delete org mode as default mode.
* Make after confirm adding header or not, change into corresponding c-mode or c++-mode.
* Added `togetherly` package.
* Added `floobits' package.
* Move cs to top in order to override the bug from csharp-mode.
* Added list of ext for c and c++ mode.
* Change copyright character.
* Change regexp for strict type langauge in oop-func file for correct variable font face.
* Added tag string and bracket sign for oop doc customization.
* Make sure curly bracket and bracket can be replace with each other in regexp, so it compatible with js doc comment style.
* Implement python docstring.
* Added variable type face issue.
* Be more specific on variable name regexp.
* Make comment with auto add oop doc comment style.
* Added cs comment doc functionality.
* Added comment style doc and java-mode match with own doc highlighting.
* Force underscore a character.
* Make defface instead of make face function in oop-func file.
* Fixed oop face regular expression highlighting issues.
* Fixed comment line break indentation issue.
* Update regular expression in oop/jsdoc for type, value and tag.
* Added `visual-regexp` package.
* Added jsdoc type highlighting functions.
* Enable column highlighting in web-mode.
* Fixed web-mode with correct PHP key bindings.
* Update key bindings note with 'auto-highlight-symbols' package.
* Update global key, so it does not conflict with 'auto-highlight- symbols' package.
* Added jcs-oop-func file for Object Oriented Programming languages highlighting.
* Move diminish to after load setting file.
* Split c and c++ mode into two file and leave cc with c and c++ common.
* Change emacs cc-mode comment face.
* Added `auto-highlight-symbol` package to diminish list.
* Added `flycheck` package to diminish list.
* Added `flymake` package to diminish list.
* Added `helm` package to diminish list.
* First draft readme.
* Change nasm key bindings indent up and down.
* Installed new package `package-build` to list.
* Installed new package `package-lint` to list.
* Added `auto-highlight-symbol` package.
* Customize package `auto-highlight-sybmol`.
* Fixed auto-highlight-symbol package face settings for my own use.
* Update `web-mode` package with prefix indentation bug fixed.
 -> #939: https://github.com/fxbois/web-mode/issues/939
* Resolve performance issue while moving the cursor around the unicode text by adding `(setq inhibit-compacting-font-caches t)` at `jcs-env.el` file.
 -> #273: https://github.com/purcell/emacs.d/issues/273
* Installed new `xwidgete` package.
* Installed new `pdf-tools` package.
* Remove some of the web-mode functionalities, seems like the original `web-mode' package had improved.
* Revert 'web-mode' functionalities because I was wrong with the bug fixed from original package.
* Remove thought was impatient mode settings.
* Update key bindings note and add category Util for handy key bindings in emacs.
* Added emacs_commands file for record important commands for future usage and note.
* Added header for emacs lisp mode.
* Revaluate the css-sort function with local variable.
* Make backup ### 2017-11-06.
* Added save-window-excursion with css sort, make sure no moving point and scroll.
* Make hide shell command proper UX.
* Added jump-to-window function to jcs-nav.
* Fixed plugin bugs css-sort cursor moves after sort occurs, preferences issue make.
* Added css-sort package.
* Added css sort key bindings to css and scss mode.
* Added css save function as default css-mode and scss-mode default save buffer functionality. Now save CSS and SCSS file with sort attribute before saving it.
* Enable auto-complete package for web-mode's minor mode when editing PHP file.
* Added vimrc-mode package.
* Added magit package.
* Added global key bindings for magit.
* Added move current line up/down functionalities.
* Added move current line up/down global key bindings.
* Update key bindings note.
* Disable yasnippet for while editing PHP file or minor mode in web-mode.
* Enable auto-complete package for global as default.
* Fixed COBOL mode auto-complete not showing issue.
* Fixed JavaScript mode auto-complete ac-timer and ac-update bugs by disable the ac-js2 minor mode.
* COBOL header format fixed data division missing.
* Makefile initial format changes.
* Change COBOL header format.
* Added initialize format for COBOL file.
* More detail and completion to cmake file format info.
* Added .ac extension to cmake mode for autotool, autoconf, automake family.
* Added cobol-mode for editing COBOL file.
* Added jcs-cbl-mode for my customization.
* Added COBOL header in header format info file.
* Fixed cs mode switch window previous key bindings.
* Make sure comment it well on indentation with scripting language in web-mode.
* Added jcs-vim-mode only for editing VimScript.
* Correct year file format in lua mode and vim mode.
* Correct filename info.
* php paste will occur web-mode get interrupt, bug fixed by renable it again.
* Added jcs-go-mode for GO programming language.
* Fix and Added jcs-align-region-or-document function for align variable cross language.
* Choose either single quote or double quote for markup.
* Include header format file once bug fixed.
* Added check current character a proper character not a sign or something else.
* Fix Web mode backward delete word and backward delete word capital.
* Added timestamp version 3 to jcs-util file.
* Change init format for SQL file.
* Added `jcs-backward-delete-word` and `jcs-backward-delete-word-capital` for PHP variable naming convention.
* Added current uppercase letter and lowercase letter check in `jcs-util` file.
* Fix file naming for some file in ex-mode directory.
* add `jcs-sql-mode.el' to ex-mode directory, meanwhile my emacs confi support editing SQL file.
* Added adaptive-wrap package for HTML editing.
* Re-enable visual-line-mode when save in web-mode.
* Added generated code for jcs-cc-mode specific for constructor and destructor.
* Added jcs-sass-mode for sass language.
* jcs python mode module test failed, fixed bugs to the better jcs python mode.
* add 'use strict' while create the js file.
* Fix move forward capital character bug.
* Update `jcs-java-mode`.
* Intalled new package `meghanada'.
* Installed new package `google-translate`.
* Installed new package `google-maps`.
* Installed new package `go-mode`.
* Installed new package `google-this`.
* Implements `python-mode` compatible with whitespace, so I don't necessary use tab for programming python.
* Update some of the `jcs-shell-mode-hook` key bindings.
* Remove package `dashboard', seems like the package no longer working.
* Change the theme color a bit.
* Installed new package `dashboard`.
* Intalled new package `powerline`.
* Installed new package `diminish`.
* Make `helm-colors' minibuffer RET insert name M-RET insert hex-code.
* Fix bug capital search backward/forward and capital delete backward.
* Added xml mode.
* Added Sublimity to emacs file.
* Added Minimap and Animate scrolling.
* Added increment/decrement transparency frame.
* Update Log file build.


<!-- Links -->

[Graphviz]: https://graphviz.org/
