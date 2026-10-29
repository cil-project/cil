#include "testharness.h"

/* The enum which exposed the missing binary literals in Intel TDX tasks. */
enum gpa_list_entry_operation_e {
  GPA_ENTRY_OP_NOP = 0b00,
  GPA_ENTRY_OP_MIGRATE = 0b01,
  GPA_ENTRY_OP_CANCEL = 0b10,
  GPA_ENTRY_OP_REMIGRATE = 0b11,
  GPA_ENTRY_OP_EXPORT_NOP_MASK = 0b01
};

/* Check both the value and the type, including constant-folded expressions. */
#define CHECK(binary, expected) do { \
  if ((binary) != (expected)) E(__LINE__); \
  if (!__builtin_types_compatible_p(__typeof__(binary), \
                                    __typeof__(expected))) E(__LINE__); \
} while (0)

int main(void)
{
  if (GPA_ENTRY_OP_NOP != 0 || GPA_ENTRY_OP_MIGRATE != 1 ||
      GPA_ENTRY_OP_CANCEL != 2 || GPA_ENTRY_OP_REMIGRATE != 3 ||
      GPA_ENTRY_OP_EXPORT_NOP_MASK != 1) E(1);

  CHECK(0b0, 0);
  CHECK(0B1, 1);
  CHECK(0b00001010, 10);
  CHECK(0B101010, 42);
  CHECK(0b1u, 1u);
  CHECK(0B1U, 1U);
  CHECK(0b1l, 1l);
  CHECK(0B1L, 1L);
  CHECK(0b1ul, 1ul);
  CHECK(0B1LU, 1LU);
  CHECK(0b1ll, 1ll);
  CHECK(0B1LL, 1LL);
  CHECK(0b1ull, 1ull);
  CHECK(0B1LLU, 1LLU);
  CHECK(0b1010 + 0B11, 13);
  CHECK(0b1 << 0B11, 8);

  /* Compare to hex so these type checks work on both ILP32 and LP64. */
  CHECK(0b1111111111111111111111111111111, 0x7fffffff);
  CHECK(0b10000000000000000000000000000000, 0x80000000);
  CHECK(0b11111111111111111111111111111111, 0xffffffff);
  CHECK(0b100000000000000000000000000000000, 0x100000000);
  CHECK(0b10000000000000000000000000000000L, 0x80000000L);
  CHECK(0b111111111111111111111111111111111111111111111111111111111111111,
        0x7fffffffffffffff);
  CHECK(0b1000000000000000000000000000000000000000000000000000000000000000,
        0x8000000000000000);
  CHECK(0B1111111111111111111111111111111111111111111111111111111111111111ULL,
        0xffffffffffffffffULL);
  CHECK(0b1000000000000000000000000000000000000000000000000000000000000000LL,
        0x8000000000000000LL);

  SUCCESS;
}
