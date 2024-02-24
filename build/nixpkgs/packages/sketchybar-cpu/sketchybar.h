#pragma once

#include <mach/mach.h>
#include <mach/mach_port.h>
#include <mach/message.h>
#include <bootstrap.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdio.h>

typedef char* env;

struct mach_message {
  mach_msg_header_t header;
  mach_msg_size_t msgh_descriptor_count;
  mach_msg_ool_descriptor_t descriptor;
};

struct mach_buffer {
  struct mach_message message;
  mach_msg_trailer_t trailer;
};

static mach_port_t g_mach_port = 0;

static inline char* env_get_value_for_key(env env, char* key) {
  uint32_t caret = 0;
  for(;;) {
    if (!env[caret]) break;
    if (strcmp(&env[caret], key) == 0)
      return &env[caret + strlen(&env[caret]) + 1];

    caret += strlen(&env[caret])
             + strlen(&env[caret + strlen(&env[caret]) + 1])
             + 2;
  }
  return (char*)"";
}

static inline mach_port_t mach_get_bs_port() {
  mach_port_name_t task = mach_task_self();

  mach_port_t bs_port;
  if (task_get_special_port(task,
                            TASK_BOOTSTRAP_PORT,
                            &bs_port            ) != KERN_SUCCESS) {
    return 0;
  }

  mach_port_t port;
  if (bootstrap_look_up(bs_port,
                        "git.felix.sketchybar",
                        &port                  ) != KERN_SUCCESS) {
    return 0;
  }

  return port;
}

static inline void mach_send_message(mach_port_t port, char* message, uint32_t len) {
  if (!message || !port) {
    return;
  }

  struct mach_message msg = { 0 };
  msg.header.msgh_remote_port = port;
  msg.header.msgh_local_port = 0;
  msg.header.msgh_id = 0;
  msg.header.msgh_bits = MACH_MSGH_BITS_SET(MACH_MSG_TYPE_COPY_SEND,
                                            MACH_MSG_TYPE_MAKE_SEND,
                                            0,
                                            MACH_MSGH_BITS_COMPLEX       );

  msg.header.msgh_size = sizeof(struct mach_message);
  msg.msgh_descriptor_count = 1;
  msg.descriptor.address = message;
  msg.descriptor.size = len * sizeof(char);
  msg.descriptor.copy = MACH_MSG_VIRTUAL_COPY;
  msg.descriptor.deallocate = false;
  msg.descriptor.type = MACH_MSG_OOL_DESCRIPTOR;

  mach_msg(&msg.header,
           MACH_SEND_MSG,
           sizeof(struct mach_message),
           0,
           MACH_PORT_NULL,
           MACH_MSG_TIMEOUT_NONE,
           MACH_PORT_NULL              );

  return;
}

static inline void sketchybar(char* message) {
  uint32_t message_length = strlen(message) + 1;
  char formatted_message[message_length + 1];

  char quote = '\0';
  uint32_t caret = 0;
  for (int i = 0; i < message_length; ++i) {
    if (message[i] == '"' || message[i] == '\'') {
      if (quote == message[i]) quote = '\0';
      else quote = message[i];
      continue;
    }
    formatted_message[caret] = message[i];
    if (message[i] == ' ' && !quote) formatted_message[caret] = '\0';
    caret++;
  }

  if (caret > 0 && formatted_message[caret] == '\0'
      && formatted_message[caret - 1] == '\0') {
    caret--;
  }

  formatted_message[caret] = '\0';
  if (!g_mach_port) g_mach_port = mach_get_bs_port();
  mach_send_message(g_mach_port,
                    formatted_message,
                    caret + 1         );
}
