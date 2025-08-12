# 编译器
CXX = g++
# 编译参数
CXXFLAGS = -std=c++20 -O2
# 目标文件名
TARGET = toyc

# 默认规则
$(TARGET): toyc.cpp
	$(CXX) $(CXXFLAGS) -o $(TARGET) toyc.cpp

# 清理规则
clean:
	rm -f $(TARGET)
