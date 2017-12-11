# -*- coding: utf-8 -*-
#!/usr/bin/env python

"""
Objective:
Calculate the average travel time for each 20-minute time window.
目标：
计算每20分钟的时间长度的车辆的平均运行时间
"""

# import necessary modules
import math
from datetime import datetime,timedelta

file_suffix = '.csv'
path = '/Users/apple/code_tool/KDD_run/KDD_python/'          # set the data directory（改文件路径）

def avgTravelTime(in_file):

    out_suffix = '_20min_avg_travel_time'
    in_file_name = in_file + file_suffix
    out_file_name = in_file.split('_')[1] + out_suffix + file_suffix
    #   training_20min_avg_travel_time.csv  或testing_20min_avg_travel_time.csv

    # Step 1: Load trajectories
    fr = open(path + in_file_name, 'r')
    fr.readline()  # skip the header(跳过首行)
    traj_data = fr.readlines()     # 将文本数据都保存到traj_data
    fr.close()                      #关闭文件
    print(traj_data[0])           #打印第一行

    # Step 2: Create a dictionary to store travel time for each route per time window
    travel_times = {}           #6种路线的字典
    # key: route_id. Value is also a dictionary of which key is the start time for the time window and value is a list of travel times
    for i in range(len(traj_data)):    #行数
        each_traj = traj_data[i].replace('"', '').split(',')    #先去除所有"再以,进行分割成列表
        intersection_id = each_traj[0]
        tollgate_id = each_traj[1]

        route_id = intersection_id + '-' + tollgate_id
        if route_id not in travel_times.keys():
            travel_times[route_id] = {}               #添加6种路线字典

        trace_start_time = each_traj[3]
        trace_start_time = datetime.strptime(trace_start_time, "%Y-%m-%d %H:%M:%S")  #将时间进行划分，单独提取出数字
        time_window_minute = math.floor(trace_start_time.minute / 20) * 20   #向下取整，确定时间窗口起点的分钟
        start_time_window = datetime(trace_start_time.year, trace_start_time.month, trace_start_time.day,
                                     trace_start_time.hour, time_window_minute, 0)    #确定时间窗口起点，精确到分钟
        tt = float(each_traj[-1]) # travel time

        if start_time_window not in travel_times[route_id].keys():
            travel_times[route_id][start_time_window] = [tt]
        else:
            travel_times[route_id][start_time_window].append(tt)
        #在目前的路线上，添加时间段的行驶时间（若存在就在该时间段添加，若不存在就重新添加）

    # Step 3: Calculate average travel time for each route per time window
    fw = open(out_file_name, 'w')
    fw.writelines(','.join(['"intersection_id"', '"tollgate_id"', '"time_window"', '"avg_travel_time"']) + '\n')
    #用逗号来连接指标名并换行
    for route in travel_times.keys():
        route_time_windows = list(travel_times[route].keys())  #该路线下的所有时间窗口
        route_time_windows.sort()                              #对#该路线下的所有时间窗口进行排序
        for time_window_start in route_time_windows:
            time_window_end = time_window_start + timedelta(minutes=20)
            tt_set = travel_times[route][time_window_start]
            avg_tt = round(sum(tt_set) / float(len(tt_set)), 2)
            out_line = ','.join(['"' + route.split('-')[0] + '"', '"' + route.split('-')[1] + '"',
                                 '"[' + str(time_window_start) + ',' + str(time_window_end) + ')"',
                                 '"' + str(avg_tt) + '"']) + '\n'
            fw.writelines(out_line)
    fw.close()

def main():

    in_file = 'trajectories(table 5)_training'
    avgTravelTime(in_file)

if __name__ == '__main__':
    main()



