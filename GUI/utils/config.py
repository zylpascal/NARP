import re
#read all config and data from OUTPUT
def countAreas(file_path, search_string="PROBABILITY DISTRIBUTIONS FOR AREA"):
    try:
        with open(file_path, "r") as file:
            file_content = file.read()
            count = file_content.count(search_string)
            return count
    except FileNotFoundError:
        print(f"找不到文件：{file_path}")
    except Exception as e:
        print(f"发生错误：{e}")

def getTimes(file_path):
    with open(file_path, 'r') as file:
    # 逐行读取文件内容
        for line in file:
        # 使用正则表达式匹配包含"FINAL RESULTS AFTER"的行
            match = re.search(r'FINAL RESULTS AFTER   (\d+)', line)
            if match:
            # 提取整数值并打印
                result = int(match.group(1))
                print(f"Found: {result}")
                return result
            
def getPoolStatistics(file_path):
    try:
        with open(file_path, "r") as file:
            file_content = file.read()
            
            # 寻找"POOL STATISTICS"和"下一个TABLE"之间的内容
            start_index = file_content.find("POOL STATISTICS")
            end_index = file_content.find("TABLE", start_index)
            
            if start_index == -1 or end_index == -1:
                print("未找到相应的标记")
                return None
            
            # 提取位于两个标记之间的文本
            table_text = file_content[start_index + len("POOL STATISTICS"):end_index].strip()
            # 将文本按行拆分成列表
            table_lines = table_text.split("\n")
            
            # 将每行的数据按空格或制表符拆分成列表
            table_data = [line.split() for line in table_lines]
            table_data = [sublist for sublist in table_data if len(sublist) >= 2]
            table_data = [[' '] + row for row in table_data]
            #给pool statistics添加表头
            table2=getAreaResults(file_path)


            for i in range(3):
                table_data.insert(i, table2[i])
                max_columns=len(table_data)
            for col_index in range(max_columns-3):
                table_data[col_index+3][1]="AVG"
            table_data[0][5]="DAILY"
            return table_data
    except FileNotFoundError:
        print(f"找不到文件：{file_path}")
    except Exception as e:
        print(f"发生错误：{e}")

#get area probability distribution by area number
def getAreaProD(file_path,area_number):
    try:
        with open(file_path, "r") as file:
            file_content = file.read()
            
            start_string = f"PROBABILITY DISTRIBUTIONS FOR AREA  {area_number}"
            end_string = "TABLE"
            start_index = file_content.find(start_string)
            end_index = file_content.find(end_string, start_index)
            
            if start_index == -1 :
                print("未找到相应的标记")
                return None
            
            # 提取位于两个标记之间的文本
            table_text = file_content[start_index + len(start_string):end_index].strip()
           # print(table_text)
            # 将文本按行拆分成列表
            table_lines = table_text.split("\n")
            
            # 将每行的数据按空格或制表符拆分成列表
            table_data = [line.split() for line in table_lines]
            table_data = [sublist for sublist in table_data if len(sublist) >= 4]
            return table_data
    except FileNotFoundError:
        print(f"找不到文件：{file_path}")
    except Exception as e:
        print(f"发生错误：{e}")
def getAreaResults(file_path):
    try:
        with open(file_path, "r") as file:
            file_content = file.read()
            
            # 寻找"POOL STATISTICS"和"下一个TABLE"之间的内容
            start_index = file_content.find("FINAL RESULTS AFTER")
            end_index = file_content.find("POOL STATISTICS", start_index)
            
            if start_index == -1 or end_index == -1:
                print("未找到相应的标记")
                return None
            
            # 提取位于两个标记之间的文本
            table_text = file_content[start_index + len("FINAL RESULTS AFTER   3576 REPLICATIONS   "):end_index].strip()
            # 将文本按行拆分成列表
            table_lines = table_text.split("\n")
            
            # 将每行的数据按空格或制表符拆分成列表
            table_data = [line.split() for line in table_lines]
            table_data = [sublist for sublist in table_data if len(sublist) >= 2]
            table_data[0].insert(3, '')
            table_data[2].insert(0, '')
            table_data[2].insert(0, '')
            max_columns=len(table_data)
            for col_index in range(max_columns-3):
                table_data[col_index+3][1]="AVG"
            table_data[0][5]="DAILY"
            return table_data
    except FileNotFoundError:
        print(f"找不到文件：{file_path}")
    except Exception as e:
        print(f"发生错误：{e}")

def getSummaryResults(file_path):
    try:
        with open(file_path, "r") as file:
            file_content = file.read()
            
            start_string = "SUMMARY OF RESULTS"
            end_string = "ERCOT"
            start_index = file_content.find(start_string)
            end_index = file_content.find(end_string, start_index)
            
            if start_index == -1 or end_index == -1:
                print("未找到相应的标记")
                return None
            
            # 提取位于两个标记之间的文本
            table_text = file_content[start_index + len(start_string):end_index].strip()
           # print(table_text)
            # 将文本按行拆分成列表
            table_lines = table_text.split("\n")
            
            # 将每行的数据按空格或制表符拆分成列表
            table_data = [line.split() for line in table_lines]
            table_data = [sublist for sublist in table_data if len(sublist) >= 2]

            #对table_data进行更直观的处理
            empty_values = ["", "", "", ""]
            table_data[0].insert(5, "")
            table_data[0].insert(7, "")
            table_data[0].insert(9, "")
            table_data[1]=empty_values+table_data[1]
            table_data[2].insert(0,"")
            table_data[2].insert(0,"")
            max_columns = max(len(row) for row in table_data[:3])
            table_data.pop(1)
            table_data[1][3]+=" "+table_data[1][4]
            del table_data[1][4]
            table_data[1][5]+=" "+table_data[1][6]
            del table_data[1][6]
            table_data[1][7]+=" "+table_data[1][8]
            del table_data[1][8]
            table_data[1][9]+=" "+table_data[1][10]
            del table_data[1][10]

            # 合并前两行
            merged_row = []
            for col_index in range(max_columns):
                merged_column = []
                for row_index in range(2):
                    if col_index < len(table_data[row_index]):
                        merged_column.append(table_data[row_index][col_index])
                merged_row.append(" ".join(merged_column))
            # 删除原列表的前三行
            table_data = table_data[2:]

            # 在原列表的开头插入合并后的行
            table_data.insert(0, merged_row)
            while len(table_data[0]) > 0 and table_data[0][-1] == '':
                table_data[0].pop()
            
            return table_data
    except FileNotFoundError:
        print(f"找不到文件：{file_path}")
    except Exception as e:
        print(f"发生错误：{e}")


def getDailyPeakLossPerYear(file_path,area_number):
    DAILY_PEAK_LOLES_PER_YEAR = []
    table_data = getAreaProD(file_path,area_number)
    for row_index, row in enumerate(table_data):
        if row_index > 0:  # 跳过第一行
            if len(row) >= 3:  # 确保每一行至少有三列数据
                extracted_row = row[:3]  # 提取前三列数据
                DAILY_PEAK_LOLES_PER_YEAR.append(extracted_row)
    return DAILY_PEAK_LOLES_PER_YEAR

def getHourlyLossPerYear(file_path,area_number):
    HOURLY_LOSES_PER_YEAR = []
    table_data = getAreaProD(file_path,area_number)
    for row_index, row in enumerate(table_data):
        if row_index > 0:  # 跳过第一行
            if len(row) >= 5:  # 确保每一行至少有五列数据
                extracted_row = [row[0], row[3], row[4]]  # 提取第1、第4和第5列的数据
                HOURLY_LOSES_PER_YEAR.append(extracted_row)
    return HOURLY_LOSES_PER_YEAR

def getAnnuallyUnservedEnergy(file_path,area_number):
    ANNUALLY_UNSERVED_ENERGY = []
    table_data = getAreaProD(file_path,area_number)
    for row_index, row in enumerate(table_data):
        if row_index== 1:  # 第一行
            first_row = row
            combined_value = first_row[5] + first_row[6]  # 合并第六和第七个元素
            first_row[5] = combined_value  # 将合并后的值替换为第六个元素
            del first_row[6]  # 删除第七个元素
        if row_index > 0:  # 跳过第一行
            if len(row) >= 3:  # 确保每一行至少有三列数据
                extracted_row = [row[5], row[6],row[7]]  # 提取第0,5,6,7列的数据
                ANNUALLY_UNSERVED_ENERGY.append(extracted_row)
    return ANNUALLY_UNSERVED_ENERGY
# 调用函数并传递文件路径

file_path = "OUTPUT"  # 替换成实际文件路径
#print(getTimes(file_path))
#table = getSummaryResults(file_path)
#table1=getPoolStatistics(file_path)
#table2=getAnnuallyUnservedEnergy(file_path,2)
#if table1 is not None:
#    # 输出提取的表格数据
#    for row in table1:
#        print(row)
