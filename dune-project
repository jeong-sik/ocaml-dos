(lang dune 3.0)
(name ocaml-dos)
(generate_opam_files true)
(package
 (name ocaml-dos)
 (synopsis "DOS/8086 emulator core - pure, deterministic, no IO")
 (description
  "8086 real-mode CPU, text video and the DOS interrupt surface. Pure library: time and IO live in the client. Target milestone: boot a turn-based DOS game (Sangokushi III) to its title.")
 (maintainers "Jeong-Sik Yun <yousleepwhen@gmail.com>")
 (authors "Jeong-Sik Yun")
 (source
  (uri git+https://github.com/jeong-sik/ocaml-dos.git))
 (license MIT)
 (depends ocaml dune yojson))
