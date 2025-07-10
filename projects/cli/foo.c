#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <time.h>

extern int errno;

int main() {
    printf("EFAULT=%d\n", EFAULT);
    printf("EMFILE=%d\n", EMFILE);
    printf("ENFILE=%d\n", ENFILE);

    printf("O_CREAT=%d\n", O_CREAT);
    printf("O_TRUNC=%d\n", O_TRUNC);

    // char filename[256];
    // snprintf(filename, sizeof(filename), "/Users/kennethg/Desktop/abra-lang/projects/cli/._abra/example%ld.ssa", time(NULL));
    // mode_t m = umask(0);
    // printf("opening file...\n");
    char* filename = "/Users/kennethg/Desktop/abra-lang/projects/cli/._abra/example2.ssa";
    if (open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0644) < 0) {
        perror("failed to create file");
        return 1;
    }
    // umask(m);

    // pid_t pid = fork();
    
    // if (pid == -1) {
    //     perror("fork failed");
    //     exit(1);
    // }
    
    // if (pid == 0) {
    //     // Child process
    //     char *args[] = {"ls", "-l", NULL};
        
    //     printf("Child: About to execute 'ls -l'\n");
    //     execvp("ls", args);
        
    //     // This line will only execute if execvp fails
    //     perror("execvp failed");
    //     exit(1);
    // } else {
    //     // Parent process
    //     int status;
    //     wait(&status);
    //     printf("Parent: Child process completed (status=%d)\n", status);
    // }
    
    return 0;
}