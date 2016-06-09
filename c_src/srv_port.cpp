#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <sys/epoll.h>
#include <unistd.h>
#include <fcntl.h>
#include <stddef.h>
#include <pthread.h>

#define MAX_EVENTS 10

static int stdi = 0, stdo = 1;

enum cmd {
    CONNECT     = 0,
    DISCONNECT  = 1,
    DATA        = 2,
    LOG         = 3
};

struct __attribute__((__packed__)) _conn {
    uint32_t len;
    uint8_t  cmd;
    uint32_t ip_addr;
    uint16_t port;
    uint32_t socket;
} conn;

struct __attribute__((__packed__)) _dconn {
    uint32_t len;
    uint8_t  cmd;
    uint32_t socket;
} dconn;

#define BUF_SIZE 0xFFFFFF
struct __attribute__((__packed__)) _data {
    uint32_t  len;
    uint8_t   cmd;
    uint32_t  socket;
    char buf[BUF_SIZE];
} data;

#define LOG_SIZE 0xFFFF
struct _log {
    uint32_t len;
    uint8_t  cmd;
    uint8_t  str[LOG_SIZE];
} log;

void dump(const char * title, void *buf, uint32_t len)
{
    printf("%s (%d) : ", title, len);
    for (uint32_t i = 0; i < len; ++i)
        printf("16#%x,", ((uint8_t*)buf)[i]);
    printf("\r\n");
}

#define L0(F) \
    sprintf((char*)log.str, "PORT [%s:%d] "F,__FUNCTION__,__LINE__);\
    log.len = htonl(strlen((char*)log.str)+1);\
    write_cmd(&log, ntohl(log.len)+sizeof(log.len));

#define L(F, ...) \
    sprintf((char*)log.str, "PORT [%s:%d] "F,__FUNCTION__,__LINE__,__VA_ARGS__);\
    log.len = htonl(strlen((char*)log.str)+1);\
    write_cmd(&log, ntohl(log.len)+sizeof(log.len));

int write_cmd(void *buf, uint32_t len)
{
    int i = 0;
    uint32_t wrote = 0;
    do {
        if ((i = write(stdo, (uint8_t*)buf + wrote, len - wrote)) <= 0)
            return i;
        wrote += i;
    } while (wrote < len);
    return len;
}

int read_cmd()
{
    int i = 0;
    uint32_t got = 0, len;
    uint8_t cmd;
    uint8_t * buf;

    if ((i = read(stdi, &len, 4)) < 4)
        return -1;
    len = ntohl(len);

    if ((i = read(stdi, &cmd, 1)) < 1)
        return -1;

    if (cmd == DATA) {
        data.len = len;
        buf = ((uint8_t*)&data) + offsetof(struct _data, socket);
    } else if (cmd == DISCONNECT) {
        dconn.len = len;
        buf = ((uint8_t*)&dconn) + offsetof(struct _dconn, socket);
    }
    len -= sizeof(uint8_t);
    do {
        if ((i = read(stdi, buf+got, len-got)) <= 0)
            return i;
        got += i;
    } while (got < len);
    return cmd;
}

uint8_t buf[BUF_SIZE];

#define THREADS 8
void print_tid(char *tid)
{
    pthread_t pt = pthread_self();
    unsigned char *ptc = (unsigned char*)(void*)(&pt);
    for (size_t i=0; i<sizeof(pt); i++) {
        sprintf(tid+i*2, "%02x", (unsigned)(ptc[i]));
    }
}

void *epoll_socks_thread(void *epollfd)
{
    struct epoll_event events[MAX_EVENTS];
    int nfds;
    int *eplfd = (int *)epollfd;
    int tidsize = sizeof(pthread_t)*2+1;
    char tid[tidsize];
    memset(tid, '\0', tidsize);
    print_tid(tid);
    fprintf(stderr, "[0x%s] eplfd %d\r\n", tid, *eplfd);

    while(true) {
        nfds = epoll_wait(*eplfd, events, MAX_EVENTS, -1);
        if (nfds == -1) {
            perror("epoll_pwait");
            exit(EXIT_FAILURE);
        }

        for (int n = 0; n < nfds; ++n) {
            while(true) {
                ssize_t len = recv(events[n].data.fd, buf, BUF_SIZE,
                                   MSG_DONTWAIT);
                if (len < 0) {
                    if (errno == EAGAIN || errno == EWOULDBLOCK)
                    break;
                } else if (len > 0) {
                    /*L0("RX :");
                    for(int i = 0; i < len; ++i) {
                        L("%02X ", data.buf[i]);
                    }
                    L0("\n");*/
                    /*len += (sizeof(data.cmd) + sizeof(data.socket));
                    data.len = htonl(len);
                    data.socket = events[n].data.fd;
                    write_cmd(&data, len + sizeof(data.len));*/
                    // direct echo back
                    if(write(events[n].data.fd, buf, len) < 0) {
                        L("ERROR FWD erlang DATA RX %d : %d bytes\n", events[n].data.fd, len);
                        perror("write : data.socket");
                        exit(EXIT_FAILURE);
                    }
                } else {
                    if (epoll_ctl(*eplfd, EPOLL_CTL_DEL,
                                  events[n].data.fd, NULL) == -1) {
                        perror("epoll_ctl: conn_sock");
                        exit(EXIT_FAILURE);
                    }
                    L0("peer closed\n");
                    break;
                }
            }
        }
    }
}

int main(int argc, char *argv[])
{
    struct epoll_event ev, events[MAX_EVENTS];
    int listen_sock, conn_sock, nfds, eplfd_lsn_sock;
    int eplfds[THREADS];
    pthread_t threads[THREADS];
    struct sockaddr_in serv_addr;

    memset(&log,   0, sizeof(log));
    memset(&conn,  0, sizeof(conn));
    memset(&data,  0, sizeof(data));
    memset(&dconn, 0, sizeof(dconn));

    log.cmd   = LOG;
    conn.cmd  = CONNECT;
    dconn.cmd = DISCONNECT;
    data.cmd  = DATA;

    conn.len  = htonl(sizeof(conn) - sizeof(conn.len));
    dconn.len = htonl(sizeof(dconn) - sizeof(dconn.len));

    listen_sock = socket(AF_INET, SOCK_STREAM, 0);
    memset(&serv_addr, '0', sizeof(serv_addr));

    serv_addr.sin_family = AF_INET;
    if(argc > 2) {
        if (inet_pton(AF_INET, argv[1], &(serv_addr.sin_addr.s_addr)) <= 0) {
            L("\nUsage: %s <listen ip> <listen port>\n", argv[0]);
            exit(0);
        }
        serv_addr.sin_port = htons(atoi(argv[2]));
    } else {
        L("\nUsage: %s <listen ip> <listen port>\n", argv[0]);
        exit(0);
    }

    bind(listen_sock, (struct sockaddr*)&serv_addr, sizeof(serv_addr));
    listen(listen_sock, 10);

    eplfd_lsn_sock = epoll_create(10);
    if (eplfd_lsn_sock == -1) {
        perror("epoll_create");
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < THREADS; ++i) {
        eplfds[i] = epoll_create(10);
        if (eplfds[i] == -1) {
            perror("epoll_create");
            exit(EXIT_FAILURE);
        }
        if(pthread_create(&(threads[i]), NULL, epoll_socks_thread, &(eplfds[i]))) {
            perror("epoll_create");
            exit(EXIT_FAILURE);
        }
    }

    // listen sock
    ev.events = EPOLLIN;
    ev.data.fd = listen_sock;
    if (epoll_ctl(eplfd_lsn_sock, EPOLL_CTL_ADD, listen_sock, &ev) == -1) {
        perror("epoll_ctl: listen_sock");
        exit(EXIT_FAILURE);
    }

    // port std input
    ev.events = EPOLLIN | EPOLLET;
    ev.data.fd = stdi;
    if (epoll_ctl(eplfd_lsn_sock, EPOLL_CTL_ADD, stdi, &ev) == -1) {
        perror("epoll_ctl: conn_sock");
        exit(EXIT_FAILURE);
    }

    struct sockaddr_in local;
    socklen_t addrlen;
    L("listening %s %s\n", argv[1], argv[2]);
    int eplfdidx = 0;
    for (;;) {
        nfds = epoll_wait(eplfd_lsn_sock, events, MAX_EVENTS, -1);
        if (nfds == -1) {
            perror("epoll_pwait");
            exit(EXIT_FAILURE);
        }

        for (int n = 0; n < nfds; ++n) {
            if (events[n].data.fd == listen_sock) {
                memset(&local, 0, sizeof(sockaddr_in));
                conn_sock = accept(listen_sock,
                                (struct sockaddr *) &local, &addrlen);
                if (conn_sock == -1) {
                    perror("accept");
                    exit(EXIT_FAILURE);
                }

                if (fcntl(conn_sock, F_SETFL, fcntl(conn_sock, F_GETFL, 0) | O_NONBLOCK)
                     < 0) {
                    perror("calling fcntl");
                    exit(EXIT_FAILURE);
                }
                ev.events = EPOLLIN | EPOLLET;
                ev.data.fd = conn_sock;
                if (epoll_ctl(eplfds[eplfdidx], EPOLL_CTL_ADD, conn_sock,
                            &ev) == -1) {
                    perror("epoll_ctl: conn_sock");
                    exit(EXIT_FAILURE);
                }

                conn.ip_addr = htonl(local.sin_addr.s_addr);
                conn.port = htons(local.sin_port);
                conn.socket = conn_sock;
                write_cmd(&conn, sizeof(conn));
                eplfdidx = (eplfdidx + 1) % THREADS;
            } else if (events[n].data.fd == stdi) {
                uint8_t cmd = read_cmd();
                switch(cmd) {
                    case DATA: {
                        uint32_t len = data.len - sizeof(data.cmd) - sizeof(data.socket);
                        if(write(data.socket, data.buf, len) < 0) {
                            L("ERROR FWD erlang DATA RX %d : %d bytes\n", data.socket, len);
                            perror("write : data.socket");
                            exit(EXIT_FAILURE);
                        }
                        break;
                    };
                    case DISCONNECT:
                        if (epoll_ctl(eplfd_lsn_sock, EPOLL_CTL_DEL,
                                      dconn.socket, NULL) == -1) {
                            perror("epoll_ctl: dconn.socket");
                            exit(EXIT_FAILURE);
                        }
                        if (close(dconn.socket) < 0) {
                            perror("close: dconn.socket");
                            exit(EXIT_FAILURE);
                        }
                        L("erlang closed %d\n", dconn.socket);
                        break;
                    default:
                        L("ERROR command %d not supported\n", cmd);
                        break;
                }
            } else {
                fprintf(stderr, "bad fd %d, expected %d and %d only\r\n",
                        events[n].data.fd, listen_sock, stdi);
            }
        }
    }
    return 0;
}
