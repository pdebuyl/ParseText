FIND_PACKAGE(Git)


IF(GIT_EXECUTABLE)
SET(MY_GIT_DIR ${SRC})
execute_process(COMMAND ${GIT_EXECUTABLE} --git-dir=${MY_GIT_DIR}/.git --work-tree=${MY_GIT_DIR} status RESULT_VARIABLE git_return OUTPUT_VARIABLE git_out)

execute_process(COMMAND ${GIT_EXECUTABLE} --git-dir=${MY_GIT_DIR}/.git --work-tree=${MY_GIT_DIR} symbolic-ref HEAD OUTPUT_VARIABLE symb_out OUTPUT_STRIP_TRAILING_WHITESPACE)
string(REGEX REPLACE "^refs/heads/" "" ParseText_symb "${symb_out}")

if(git_out MATCHES "working directory clean")

execute_process(COMMAND ${GIT_EXECUTABLE} --git-dir=${MY_GIT_DIR}/.git --work-tree=${MY_GIT_DIR} log -n1 --format=%H OUTPUT_VARIABLE ParseText_commit OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND ${GIT_EXECUTABLE} --git-dir=${MY_GIT_DIR}/.git --work-tree=${MY_GIT_DIR} describe OUTPUT_VARIABLE ParseText_version OUTPUT_STRIP_TRAILING_WHITESPACE)

else(git_out MATCHES "working directory clean")

set(ParseText_commit "unknown rev. or working dir. not clean")

endif(git_out MATCHES "working directory clean")

set(ParseText_commit "${ParseText_symb} / ${ParseText_commit}")

execute_process(COMMAND ${GIT_EXECUTABLE} --git-dir=${MY_GIT_DIR}/.git --work-tree=${MY_GIT_DIR} describe OUTPUT_VARIABLE ParseText_version OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND ${GIT_EXECUTABLE} --git-dir=${MY_GIT_DIR}/.git --work-tree=${MY_GIT_DIR} log -n1 --format=%aD OUTPUT_VARIABLE ParseText_date OUTPUT_STRIP_TRAILING_WHITESPACE)
SET(ParseText_version_date "${ParseText_version} ${ParseText_date}")

execute_process(COMMAND "uname" "-nsm" OUTPUT_VARIABLE ParseText_machine OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND "basename" "${CMAKE_Fortran_COMPILER}" OUTPUT_VARIABLE ParseText_compiler OUTPUT_STRIP_TRAILING_WHITESPACE)
SET(ParseText_compiler "${ParseText_compiler} ${CMAKE_Fortran_FLAGS}")
configure_file(${SRC}/version.h.in ${F95_MOD_DIR}/version.h @ONLY)

else(GIT_EXECUTABLE)

configure_file(${SRC}/f95/version.h.git.in ${F95_MOD_DIR}/version.h COPYONLY)

endif(GIT_EXECUTABLE)



