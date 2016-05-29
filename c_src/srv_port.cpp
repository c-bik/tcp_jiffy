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
//#include <stdint.h>

#define MAX_EVENTS 10
#define BUF_SIZE 1024

static int stdi = 0, stdo = 1;

union _buffer {
    uint32_t len;
    uint8_t buf[0xFFFFFFFF+4];
} rx_buf, tx_buf;

#define CONNECT 0
struct _connect_cmd {
    uint8_t cmd;
    uint32_t ip_addr;
    uint16_t port;
    uint32_t socket;
} connect_cmd;

#define DISCONNECT 1
struct _disconnect_cmd {
    uint8_t cmd;
    uint32_t socket;
} disconnect_cmd;

#define DATA 3
struct _data_cmd {
    uint8_t cmd;
    uint32_t socket;
    union {
        uint32_t len;
        uint8_t * buf;
    };
} data_cmd;

#define LOG 4
struct _log_cmd {
    uint8_t cmd;
    uint8_t buf[0xFFFFFF];
} log_cmd;

int read_cmd()
{
    int i = 0;
    uint32_t got = 0, len = 4;
    if ((i = read(stdi, &rx_buf.buf[got], 4)) < 4)
        return -1;
    got+=4;
    rx_buf.len = ntohl(*((uint32_t*)&rx_buf.buf[0]));
    len += rx_buf.len;
    do {
        if ((i = read(stdi, &rx_buf.buf[got], len-got)) <= 0)
            return i;
        got += i;
    } while (got < len);
    return len;
}

int write_cmd()
{
    int i = 0;
    uint32_t wrote = 0, len = htonl(tx_buf.len);
    tx_buf.len = len;
    len += 4;
    do {
        if ((i = write(stdo, &tx_buf.buf[wrote], len - wrote)) <= 0)
            return i;
        wrote += i;
    } while (wrote < len);

    return len;
}

#define L0(F) \
    sprintf((char*)log_cmd.buf, "[%s:%d] "F,__FUNCTION__,__LINE__);\
    tx_buf.len = strlen((char*)log_cmd.buf)+1;\
    memcpy(tx_buf.buf+4, &log_cmd, tx_buf.len+1);\
    write_cmd();\

#define L(F, ...) \
    sprintf((char*)log_cmd.buf, "[%s:%d] "F,__FUNCTION__,__LINE__,__VA_ARGS__);\
    tx_buf.len = strlen((char*)log_cmd.buf)+1;\
    memcpy(tx_buf.buf+4, &log_cmd, tx_buf.len+1);\
    write_cmd();\

int main(int argc, char *argv[])
{
    struct epoll_event ev, events[MAX_EVENTS];
    int listen_sock, conn_sock, nfds, epollfd;
    struct sockaddr_in serv_addr;
    pid_t childPid;

    connect_cmd.cmd = CONNECT;
    disconnect_cmd.cmd = DISCONNECT;
    data_cmd.cmd = DATA;
    log_cmd.cmd = LOG;

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

    L("listening %s %s\n", argv[1], argv[2]);

    epollfd = epoll_create(10);
    if (epollfd == -1) {
        perror("epoll_create");
        exit(EXIT_FAILURE);
    }

    ev.events = EPOLLIN;
    ev.data.fd = listen_sock;
    if (epoll_ctl(epollfd, EPOLL_CTL_ADD, listen_sock, &ev) == -1) {
        perror("epoll_ctl: listen_sock");
        exit(EXIT_FAILURE);
    }

    struct sockaddr_in local;
    socklen_t addrlen;
    char buf[BUF_SIZE];
    memset(buf, 0, BUF_SIZE);
    L0("starting epoll loop\n");
    for (;;) {
        nfds = epoll_wait(epollfd, events, MAX_EVENTS, -1);
        if (nfds == -1) {
            perror("epoll_pwait");
            exit(EXIT_FAILURE);
        }

        for (int n = 0; n < nfds; ++n) {
            if (events[n].data.fd == listen_sock) {
                conn_sock = accept(listen_sock,
                                (struct sockaddr *) &local, &addrlen);
                if (conn_sock == -1) {
                    perror("accept");
                    exit(EXIT_FAILURE);
                }

                connect_cmd.ip_addr = local.sin_addr.s_addr;
                connect_cmd.port = ntohs(local.sin_port);
                connect_cmd.socket = conn_sock;
                tx_buf.len = sizeof(connect_cmd);
                memcpy(tx_buf.buf+4, &connect_cmd, tx_buf.len);
                write_cmd();

                if (fcntl(conn_sock, F_SETFL, fcntl(conn_sock, F_GETFL, 0) | O_NONBLOCK)
                     < 0) {
                    perror("calling fcntl");
                    exit(EXIT_FAILURE);
                }
                ev.events = EPOLLIN | EPOLLET;
                ev.data.fd = conn_sock;
                if (epoll_ctl(epollfd, EPOLL_CTL_ADD, conn_sock,
                            &ev) == -1) {
                    perror("epoll_ctl: conn_sock");
                    exit(EXIT_FAILURE);
                }
            } else {
                while(true) {
                    ssize_t len = recv(events[n].data.fd, buf, BUF_SIZE, MSG_DONTWAIT);
                    if (len < 0) {
                        if (errno == EAGAIN || errno == EWOULDBLOCK)
                        break;
                    } else if (len > 0) {
                        L0("RX :");
                        for(int i = 0; i < len; ++i) {
                            L("%02X ", buf[i]);
                        }
                        L0("\n");
                    } else {
                        L0("peer closed\n");
                        break;
                    }
                }
            }
        }
    }
    return 0;
}
