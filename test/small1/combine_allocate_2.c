typedef unsigned int size_t;
struct __Q2_4_STL18allocator__tm__2_w;
typedef long __Q3_4_STL18allocator__tm__2_w10value_type;
typedef size_t __Q3_4_STL18allocator__tm__2_w9size_type;
extern void *trusted_cast (void *p);
void
  *_M_allocate__Q2_4_STL37__node_alloc__tm__17_XCbL_1_1XCiL_1_0SFUi_Pv (size_t
									__9936_53___n);
__inline static void
  *allocate__Q2_4_STL37__node_alloc__tm__17_XCbL_1_1XCiL_1_0SFUi_Pv (size_t
								     __10884_34___n);
__inline static void *__stl_new__4_STLFUi (size_t __10515_33___n);
__inline static void *
allocate__Q2_4_STL37__node_alloc__tm__17_XCbL_1_1XCiL_1_0SFUi_Pv (size_t
								  __10884_34___n)
{
  void *tmp___1;
  {
    if (__10884_34___n > 128U)
      {
	tmp___1 = __stl_new__4_STLFUi (__10884_34___n);
      }
    else
      {
	tmp___1 =
	  _M_allocate__Q2_4_STL37__node_alloc__tm__17_XCbL_1_1XCiL_1_0SFUi_Pv
	  (__10884_34___n);
      }
    return (tmp___1);
  }
}
__inline static int
sizeof_value_type__Q2_4_STL18allocator__tm__2_wSFv_i (void) { return 3; }
__inline static long
  *allocate__Q2_4_STL18allocator__tm__2_wCFUiPCv_PZ1Z (struct
						       __Q2_4_STL18allocator__tm__2_w
						       const *this,
						       __Q3_4_STL18allocator__tm__2_w9size_type
						       __11447_27___n,
						       void const
						       *__T136330768);
__inline static void
  *allocate__Q2_4_STL37__node_alloc__tm__17_XCbL_1_1XCiL_1_0SFUi_Pv___0
  (size_t __11367_34___n);
long *globalPointer__Q2_4_STL18allocator__tm__2_w;
__inline static long *
allocate__Q2_4_STL18allocator__tm__2_wCFUiPCv_PZ1Z (struct
						    __Q2_4_STL18allocator__tm__2_w
						    const *this,
						    __Q3_4_STL18allocator__tm__2_w9size_type
						    __11447_27___n,
						    void const *__T136330768)
{
  long *__11450_10_ret;
  int tmp___2;
  void *tmp___3;
  {
    if (__11447_27___n)
      {
	tmp___2 = sizeof_value_type__Q2_4_STL18allocator__tm__2_wSFv_i ();
	tmp___3 =
	  allocate__Q2_4_STL37__node_alloc__tm__17_XCbL_1_1XCiL_1_0SFUi_Pv___0
	  (__11447_27___n * (unsigned int) tmp___2);
	__11450_10_ret = (long *) trusted_cast (tmp___3);
      }
    else
      {
	__11450_10_ret = (__Q3_4_STL18allocator__tm__2_w10value_type *) 0;
      }
    globalPointer__Q2_4_STL18allocator__tm__2_w = __11450_10_ret;
    __11450_10_ret = globalPointer__Q2_4_STL18allocator__tm__2_w;
    return (__11450_10_ret);
  }
}
__inline static void *
allocate__Q2_4_STL37__node_alloc__tm__17_XCbL_1_1XCiL_1_0SFUi_Pv___0 (size_t
								      __11367_34___n)
{
  void *tmp___1;
  {
    if (__11367_34___n > 128U)
      {
	tmp___1 = __stl_new__4_STLFUi (__11367_34___n);
      }
    else
      {
	tmp___1 =
	  _M_allocate__Q2_4_STL37__node_alloc__tm__17_XCbL_1_1XCiL_1_0SFUi_Pv
	  (__11367_34___n);
      }
    return (tmp___1);
  }
}


void
  *_M_allocate__Q2_4_STL37__node_alloc__tm__17_XCbL_1_1XCiL_1_0SFUi_Pv (size_t
									__9936_53___n) {}

__inline static void *__stl_new__4_STLFUi (size_t __10515_33___n) {}

int
main ()
{
  allocate__Q2_4_STL37__node_alloc__tm__17_XCbL_1_1XCiL_1_0SFUi_Pv___0(0);
  return 0;
}
