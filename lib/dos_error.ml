(* INT 21h 에러의 확장 정보 — MS-DOS 4.0 src/DOS/MS_TABLE.ASM 의
   I21_MAP_E_TAB·ERR_TABLE_21, 번호는 src/INC/ERROR.INC. *)

type error_class =
  | Out_of_resource | Temporary | Authorization | Internal | Hardware_failure
  | System_failure | Application | Not_found | Bad_format | Locked | Media
  | Already_exists | Unknown_class

type action = Retry | Delay_retry | User | Abort | Panic | Ignore | Interrupt_retry

type locus = Unknown_locus | Disk | Network | Serial_device | Memory

let class_code = function
  | Out_of_resource -> 1
  | Temporary -> 2
  | Authorization -> 3
  | Internal -> 4
  | Hardware_failure -> 5
  | System_failure -> 6
  | Application -> 7
  | Not_found -> 8
  | Bad_format -> 9
  | Locked -> 10
  | Media -> 11
  | Already_exists -> 12
  | Unknown_class -> 13

let action_code = function
  | Retry -> 1
  | Delay_retry -> 2
  | User -> 3
  | Abort -> 4
  | Panic -> 5
  | Ignore -> 6
  | Interrupt_retry -> 7

let locus_code = function
  | Unknown_locus -> 1
  | Disk -> 2
  | Network -> 3
  | Serial_device -> 4
  | Memory -> 5

(* ERROR.INC *)
let invalid_function = 1
let file_not_found = 2
let path_not_found = 3
let too_many_open_files = 4
let access_denied = 5
let invalid_handle = 6
let arena_trashed = 7
let not_enough_memory = 8
let invalid_block = 9
let bad_environment = 10
let bad_format = 11
let invalid_access = 12
let invalid_data = 13
let invalid_drive = 15
let current_directory = 16
let not_same_device = 17
let no_more_files = 18
let not_dos_disk = 26
let sharing_violation = 32
let lock_violation = 33
let sharing_buffer_exceeded = 36
let handle_eof = 38
let handle_disk_full = 39
let not_supported = 50
let file_exists = 80
let cannot_make = 82
let fail_i24 = 83
let out_of_structures = 84
let already_assigned = 85
let invalid_password = 86
let invalid_parameter = 87
let sys_comp_not_loaded = 90

(* ERR_TABLE_21. locus 가 None 이면 0FFh — 기능이 이미 정한 값을 둔다. *)
type info = { error_class : error_class; action : action; locus : locus option }

let table =
  let e error_class action locus = { error_class; action; locus } in
  [ invalid_function, e Application Abort None;
    file_not_found, e Not_found User (Some Disk);
    path_not_found, e Not_found User (Some Disk);
    too_many_open_files, e Out_of_resource Abort (Some Unknown_locus);
    access_denied, e Authorization User None;
    invalid_handle, e Application Abort (Some Unknown_locus);
    arena_trashed, e Application Panic (Some Memory);
    not_enough_memory, e Out_of_resource Abort (Some Memory);
    invalid_block, e Application Abort (Some Memory);
    bad_environment, e Application Abort (Some Memory);
    bad_format, e Bad_format User (Some Unknown_locus);
    invalid_access, e Application Abort (Some Unknown_locus);
    invalid_data, e Bad_format Abort (Some Unknown_locus);
    invalid_drive, e Not_found User (Some Disk);
    current_directory, e Authorization User (Some Disk);
    not_same_device, e Unknown_class User (Some Disk);
    no_more_files, e Not_found User (Some Disk);
    file_exists, e Already_exists User (Some Disk);
    sharing_violation, e Locked Delay_retry (Some Disk);
    lock_violation, e Locked Delay_retry (Some Disk);
    out_of_structures, e Out_of_resource Abort None;
    invalid_password, e Authorization User (Some Unknown_locus);
    cannot_make, e Out_of_resource Abort (Some Disk);
    not_supported, e Bad_format User (Some Network);
    already_assigned, e Already_exists User (Some Network);
    invalid_parameter, e Bad_format User (Some Unknown_locus);
    fail_i24, e Unknown_class Abort (Some Unknown_locus);
    sharing_buffer_exceeded, e Out_of_resource Abort (Some Memory);
    handle_eof, e Out_of_resource Abort (Some Unknown_locus);
    handle_disk_full, e Out_of_resource Abort (Some Unknown_locus);
    sys_comp_not_loaded, e Unknown_class Abort (Some Disk) ]

let info code = List.assoc_opt code table

(* I21_MAP_E_TAB: 기능마다 돌려줄 수 있는 코드. 목록에 없는 코드는 목록의
   마지막 코드로 바꿔 돌려준다(ETAB_LK 의 LOOP 가 마지막 값에서 끝난다).
   목록이 없는 기능은 코드를 그대로 돌려준다. *)
let allowed =
  [ 0x38, [ invalid_function; file_not_found ];
    0x39, [ path_not_found; file_not_found; access_denied ];
    0x3A, [ current_directory; path_not_found; file_not_found; access_denied ];
    0x3B, [ file_not_found; path_not_found ];
    0x3C, [ path_not_found; file_not_found; too_many_open_files; access_denied ];
    0x3D, [ path_not_found; file_not_found; invalid_access; too_many_open_files;
            not_dos_disk; access_denied ];
    0x3E, [ invalid_handle ];
    0x3F, [ invalid_handle; access_denied ];
    0x40, [ invalid_handle; access_denied ];
    0x41, [ path_not_found; file_not_found; access_denied ];
    0x42, [ invalid_handle; invalid_function ];
    0x43, [ path_not_found; file_not_found; invalid_function; access_denied ];
    0x44, [ invalid_drive; invalid_data; invalid_function; invalid_handle; access_denied ];
    0x45, [ invalid_handle; too_many_open_files ];
    0x46, [ invalid_handle; too_many_open_files ];
    0x47, [ not_dos_disk; invalid_drive ];
    0x48, [ arena_trashed; not_enough_memory ];
    0x49, [ arena_trashed; invalid_block ];
    0x4A, [ arena_trashed; invalid_block; not_enough_memory ];
    0x4B, [ path_not_found; invalid_function; file_not_found; too_many_open_files;
            bad_format; bad_environment; not_enough_memory; access_denied ];
    0x4E, [ path_not_found; file_not_found; no_more_files ];
    0x4F, [ no_more_files ];
    0x56, [ not_same_device; path_not_found; file_not_found; current_directory;
            access_denied ];
    0x57, [ invalid_handle; not_enough_memory; invalid_data; invalid_function ];
    0x58, [ invalid_function ];
    0x5A, [ path_not_found; file_not_found; too_many_open_files; access_denied ];
    0x5B, [ file_exists; path_not_found; file_not_found; too_many_open_files;
            access_denied ];
    0x5C, [ invalid_handle; invalid_function; sharing_buffer_exceeded; lock_violation ];
    0x65, [ invalid_function; file_not_found ];
    0x66, [ invalid_function; file_not_found ];
    0x68, [ invalid_handle ];
    0x67, [ too_many_open_files; not_enough_memory; invalid_function ];
    0x6C, [ path_not_found; file_not_found; invalid_access; too_many_open_files;
            file_exists; not_enough_memory; not_dos_disk; invalid_data;
            invalid_function; access_denied ];
    0x69, [ invalid_drive; invalid_data; invalid_function; access_denied ] ]

let returned_code ~fn code =
  match List.assoc_opt fn allowed with
  | None -> code
  | Some codes ->
    if List.mem code codes then code
    else List.nth codes (List.length codes - 1)
