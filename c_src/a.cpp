#include <stdio.h>
#include <arpa/inet.h>
#include <string.h>

enum cmd {
    CONNECT     = 0,
    DISCONNECT  = 1,
    DATA        = 2,
    LOG         = 3
};

struct _conn {
    uint32_t len;
    uint8_t  cmd;
    uint32_t ip_addr;
    uint16_t port;
    uint32_t socket;
} conn;

struct _dconn {
    uint32_t len;
    uint8_t  cmd;
    uint32_t socket;
} dconn;

struct _log {
    uint32_t len;
    uint8_t  cmd;
    uint8_t  str[0xFF];
} log;

void dump(const char * title, void *buf, uint32_t len)
{
    printf("%s (%d) : ", title, len);
    for (uint32_t i = 0; i < len; ++i)
        printf("16#%x,", ((uint8_t*)buf)[i]);
    printf("\r\n");
}

#define L(F, ...) \
    sprintf((char*)log.str, "[%s:%d] "F,__FUNCTION__,__LINE__,__VA_ARGS__);\
    log.len = strlen((char*)log.str)+1;

int main(int argc, char *argv[])
{
    memset(&log,   0, sizeof(log));
    memset(&conn,  0, sizeof(conn));
    memset(&dconn, 0, sizeof(conn));

    log.cmd   = LOG;
    conn.cmd  = CONNECT;
    dconn.cmd = DISCONNECT;

    conn.len  = sizeof(conn) - sizeof(conn.len);
    dconn.len = sizeof(dconn) - sizeof(dconn.len);

    printf("log     %d\r\n",  sizeof(log));
    printf("conn    %d\r\n",  sizeof(conn));
    printf("dconn   %d\r\n",  sizeof(dconn));

    L("test %d\r\n", 1);
    dump("log", &log, log.len+sizeof(log.len));

    conn.ip_addr = htonl(0x03040506);
    conn.port = htons(0x0102);
    conn.socket = 0x0708090A;
    dump("connect", &conn, sizeof(conn));

    dconn.socket = 0x0708090A;
    dump("disconnect", &dconn, sizeof(dconn));

    return 0;
}
