# 编译器
CXX = g++
# 编译参数
CXXFLAGS = -std=c++20 -O2
# 目标文件名
TARGET = ToyC

# 默认规则
$(TARGET): ToyC.cpp
	$(CXX) $(CXXFLAGS) -o $(TARGET) ToyC.cpp

# 清理规则
clean:
	rm -f $(TARGET)

