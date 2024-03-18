#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gsl/gsl_multimin.h>

#define maxLimit 30
#define integralNum 1000

/*
* ���б����г���tSum��tSumMax��stationFlowNum��stationSpareNum��beta��
* departTimeResult��arriveTimeResult���ǿɱ༭���ݼ�ǰ�����
*/
int vehicleNum = 28; // ������
int stationNum = 29; // ��վ��
double departTime[maxLimit][maxLimit];//��վʱ��
double arriveTime[maxLimit][maxLimit];//��վʱ��
//double departTimePre[maxLimit][maxLimit] = {//��Ϊ�г�����Ϊվ�㣬Ԫ��Ϊʱ��
//    { 60, 160, 260, 360, 460, 560, 660, 760, 860, 960, 1060, 1160, 1260 },
//    { 360, 460, 560, 660, 760, 860, 960, 1060, 1160, 1260, 1360, 1460, 1560 },
//};//ԭ��վʱ��
//double arriveTimePre[maxLimit][maxLimit] = {
//    { 30, 130, 230, 330, 430, 530, 630, 730, 830, 930, 1030, 1130, 1230 },
//    { 330, 430, 530, 630, 730, 830, 930, 1030, 1130, 1230, 1330, 1430, 1530 },
//};//ԭ��վʱ��
double departTimePre[maxLimit][maxLimit];//ԭ��վʱ��
double arriveTimePre[maxLimit][maxLimit] = {//��Ϊ�г�����Ϊվ�㣬Ԫ��Ϊʱ��
    { -1, -1, -1, -1, -1, 3527 },
    { 2777 }, 
    { -1, -1, -1, -1, -1, 4028 },
    { 3372 }, 
    { -1, -1, -1, -1, -1, 4711 },
    { 3911 }, 
    { -1, -1, -1, -1, -1, 5116 },
    { 4281 },
    { -1, -1, -1, -1, -1, 5596 },
    { 4677 },
    { -1, -1, -1, -1, -1, 6076 },
    { 5312 },
    { -1, -1, -1, -1, -1, 6466 },
    { 5727 },
    { 5961 },
    { 6117 },
    { 6298 },
    { 6597 },
    { 6722 },
    { -1, -1, -1, -1, -1, 7786 },
    { 7077 },
    { 7294 },
    { 7557 },
    { 7707 },
    { 8067 },
    { 8307 },
    { 8547 },
    { 8787 },
};//ԭ��վʱ��
int changeable[maxLimit][maxLimit];//������ͻ����������ǿ�Ԥ���Դ����
double dwellTime[maxLimit] = {
    324, 25, 30, 25, 25, 45,
    25, 55, 25, 30, 30, 25,
    30, 35, 30, 55, 30,
    45, 35, 55, 30, 30, 25,
    50, 40, 50, 30, 25, 45
};//ͣվʱ��
double dwellSpareTime[maxLimit] = {
    324, 25, 30, 25, 25, 45,
    25, 55, 25, 30, 30, 25,
    30, 35, 30, 55, 30,
    45, 35, 55, 30, 30, 25,
    50, 40, 50, 30, 25, 45
};//���ó�ͣվʱ��
double runTime[maxLimit] = {
    196, 106, 125, 167, 138, 134,
    96, 109, 105, 105, 94, 99,
    101, 95, 91, 88, 86, 192,
    150, 114, 135, 87, 210,
    144, 118, 97, 93, 172,
};//����һվ������ʱ��
double headwayT = 180;//ht
double headwayS = 600;//hs
double tSum[maxLimit];
double tSumMax;
double peopleNumP[maxLimit][maxLimit];//�ȴ��˿���
double peopleNumPP[maxLimit][maxLimit];//����Cmax�ĳ˿���
double peopleNumS[maxLimit][maxLimit];//�����˿���
int stationFlowID[1] = { 22 };//�������վid����˳��д
int stationFlowNum;//�������վ����
double Cmax[maxLimit];
double alphaC = 0;
int stationSpareID[1] = { 0 };//���ó���Ͷ�ų�վID����˳��д
int stationSpareNum;//���ó���Ͷ�ų�վ����
float alpha = 0;
float beta = 0;
float gamma = 1;
double departTimeResult[maxLimit][maxLimit][maxLimit];
double arriveTimeResult[maxLimit][maxLimit][maxLimit];
double departTimeResultTemp[maxLimit][maxLimit];
double arriveTimeResultTemp[maxLimit][maxLimit];
char stationSpared[10] = "";
char vehicleSpared[10] = "";
int stationSpareidEvery;
int vehicleSpareidEvery;
int depth = 0;
double startendTime[2] = { 7200, 9000 };

double PeopleFlow(double t, int idStation) {//������������Ϊÿ��վ������Լ��Ĺ���t��һԪ����
    if (t >= startendTime[0] && t < startendTime[1]) {
        if (idStation == 22) {
            if (t >= 7200 && t < 7500) {
                return 6.5;
            }
            else if (t >= 7500 && t < 7800) {
                return 6.81;
            }
            else if (t >= 7800 && t < 8100) {
                return 7.16;
            }
            else if (t >= 8100 && t < 8400) {
                return 5.93;
            }
            else if (t >= 8400 && t < 8700) {
                return 4.64;
            }
            else if (t >= 8700 && t < 9000) {
                return 4.51;
            }
        }
        else if (idStation == 23 || idStation == 24) {
            if (t >= 7200 && t < 7500) {
                return 5.08;
            }
            else if (t >= 7500 && t < 7800) {
                return 4.48;
            }
            else if (t >= 7800 && t < 8100) {
                return 4.61;
            }
            else if (t >= 8100 && t < 8400) {
                return 3.59;
            }
            else if (t >= 8400 && t < 8700) {
                return 2.96;
            }
            else if (t >= 8700 && t < 9000) {
                return 2.44;
            }
        }
        return 1 / 10;
    }
    else {
        return 0;
    }
}

double Integral(double (*f)(double, int), double a, double b, double n, int idStation) {//�����
    double h = (b - a) / n;
    double result = 0.0;
    for (int i = 0; i < n; i++) {
        double value = f(a + (double)i * h, idStation);
        result += value * h;
    }
    return result;
}

double IntegralSolve(double (*f)(double, int), double a, double b, double n, int idStation, double value) {
    double h = (b - a) / n;
    double result = 0.0;
    for (int i = 0; i < n; i++) {
        result += f(a + (double)i * h, idStation) * h;
        if (result >= value) {
            return a + (double)i * h;
        }
    }
    return b;
}

double Inspect(double time[maxLimit][maxLimit], int i, int idk, int dir) {
    if (time[i][idk] != -1) {
        return time[i][idk];
    }
    else {
        for (int ii = i + dir * 1; ii >= 0 && ii < vehicleNum + 1; ii = ii + dir * 1) {
            if (time[ii][idk] != -1) {
                return time[ii][idk];
            }
        }
        return -1;
    }
}

double PeolpleNumStation_1(double t, int idStation) {
    for (int i = 0; i < vehicleNum + 1; i++) {
        if (i == 0) {
            if (arriveTime[i][idStation] > t) {
                return Integral(PeopleFlow, 0, t, integralNum, idStation);
            }
            else if (arriveTime[i][idStation] <= t && departTime[i][idStation] > t) {
                return 0;
            }
        }
        else {
            double departTimei_1 = Inspect(departTime, i - 1, idStation, -1);
            if (departTimei_1 != -1) {
                if (arriveTime[i][idStation] > t && departTimei_1 <= t) {
                    return Integral(PeopleFlow, departTimei_1, t, integralNum, idStation);
                }
                else if (arriveTime[i][idStation] <= t && departTime[i][idStation] > t) {
                    return 0;
                }
            }
            else {
                if (arriveTime[i][idStation] > t) {
                    return Integral(PeopleFlow, 0, t, integralNum, idStation);
                }
                else if (arriveTime[i][idStation] <= t && departTime[i][idStation] > t) {
                    return 0;
                }
            }
        }
    }
    return 0;
}

void PeolpleNumStation(double departTime[maxLimit][maxLimit], double arriveTime[maxLimit][maxLimit]) {
    for (int i = 0; i < vehicleNum + 1; i++) {//����һ�����ó�
        for (int j = 0; j < stationNum; j++) {
            if (i == 0) {
                if (departTime[i][j] != -1) {
                    peopleNumP[i][j] = Integral(PeopleFlow, 0.0, departTime[i][j], integralNum, j);
                }
                else {
                    peopleNumP[i][j] = 0;
                }
            }
            else {
                if (departTime[i][j] == -1) {
                    peopleNumP[i][j] = 0;
                }
                else {
                    double aTime = Inspect(departTime, i - 1, j, -1);
                    double bTime = Inspect(arriveTime, i, j, 1);
                    if (aTime == -1) {
                        peopleNumP[i][j] = Integral(PeopleFlow, 0, bTime, integralNum, j);
                    }
                    else {
                        peopleNumP[i][j] = Integral(PeopleFlow, aTime, bTime, integralNum, j);
                    }
                }
            }

            if (peopleNumP[i][j] > alphaC * Cmax[j]) {
                peopleNumPP[i][j] = peopleNumP[i][j] - alphaC * Cmax[j];
            }
            else {
                peopleNumPP[i][j] = 0;
            }
        }
    }
}

double Constraint(int vehicleSpareid, int stationSpareid) {//����������
    double value = 0;
    for (int i = 0; i < vehicleNum + 1; i++) {
        if (i != 0) {
            for (int j = 0; j < stationNum; j++) {
                if (j < stationSpareid) {
                    if (i != vehicleSpareid && i - 1 != vehicleSpareid) {
                        if (arriveTime[i][j] - arriveTime[i - 1][j] < headwayT) {
                            value += headwayT - (arriveTime[i][j] - arriveTime[i - 1][j]);
                        }
                    }
                    if (i == vehicleSpareid) {
                        if (arriveTime[i + 1][j] - arriveTime[i - 1][j] > 1.5 * headwayS) {
                            value += arriveTime[i + 1][j] - arriveTime[i - 1][j] - 1.5 * headwayS;
                        }
                    }
                }
                else {
                    if (arriveTime[i][j] - arriveTime[i - 1][j] < headwayT) {
                        value += headwayT - (arriveTime[i][j] - arriveTime[i - 1][j]);
                    }
                }
            }
            if (i == vehicleSpareid) {
                if (arriveTime[i][stationSpareid] < departTime[i - 1][stationSpareid])
                {
                    value += departTime[i - 1][stationSpareid] - arriveTime[i][stationSpareid];
                }
            }
            else {
                if (arriveTime[i][0] < departTime[i - 1][0])
                {
                    value += departTime[i - 1][0] - arriveTime[i][0];
                }
            }
        }
        if (i == vehicleSpareid) {
            if (arriveTime[i][stationSpareid] < 0)
            {
                value += -arriveTime[i][stationSpareid];
            }
        }
        else {
            if (arriveTime[i][0] < 0)
            {
                value += -arriveTime[i][0];
            }
        }
    }
    return value * 1e100;
}

/*
* ��Ŀ�꺯��t
*/
double Function_tsum(const gsl_vector* x, void* params) {
    int stationSpareid = ((int*)params)[0];
    int vehicleSpareid = ((int*)params)[1];
    for (int i = 0; i < vehicleNum + 1; i++) {
        for (int j = 0; j < stationNum; j++) {
            arriveTime[i][j] = 0;
            departTime[i][j] = 0;
        }
    }
    double constraintValue = 0;

    //����ͼԼ��
    for (int i = 0; i < vehicleNum + 1; i++) {
        for (int j = 0; j < stationNum; j++) {
            if (i < vehicleSpareid) {
                arriveTime[i][j] = arriveTimePre[i][j];
                departTime[i][j] = departTimePre[i][j];
            }
            else if (i == vehicleSpareid) {
                if (j < stationSpareid) {
                    arriveTime[i][j] = -1;
                    departTime[i][j] = -1;
                }
                else if (j == stationSpareid) {
                    arriveTime[i][j] = gsl_vector_get(x, 0);
                    departTime[i][j] = arriveTime[i][j];
                }
                else {
                    if (j < stationFlowID[0]) {//�������վǰ��ͣ
                        arriveTime[i][j] = departTime[i][j - 1] + runTime[j - 1];
                        departTime[i][j] = arriveTime[i][j];
                    }
                    else {
                        arriveTime[i][j] = departTime[i][j - 1] + runTime[j - 1];
                        departTime[i][j] = arriveTime[i][j] + dwellSpareTime[j];
                    }
                }
            }
            else if (i > vehicleSpareid) {
                //if (changeable[i - depth][j] == 1) {
                //    if (j == 0) {
                //        arriveTime[i][j] = gsl_vector_get(x, ((size_t)i - vehicleSpareid));
                //        departTime[i][j] = arriveTime[i][j] + dwellTime[j];
                //    }
                //    else {
                //        arriveTime[i][j] = departTime[i][j - 1] + runTime[j - 1];
                //        departTime[i][j] = arriveTime[i][j] + dwellTime[j];
                //    }
                //}
                //else if (changeable[i - depth][j] == 0) {
                //    arriveTime[i][j] = arriveTimePre[i - depth][j];
                //    departTime[i][j] = departTimePre[i - depth][j];
                //}
                if (j == 0) {
                    arriveTime[i][j] = gsl_vector_get(x, ((size_t)i - vehicleSpareid));
                    departTime[i][j] = arriveTime[i][j] + dwellTime[j];
                }
                else {
                    arriveTime[i][j] = departTime[i][j - 1] + runTime[j - 1];
                    departTime[i][j] = arriveTime[i][j] + dwellTime[j];
                }
            }
        }
    }
    constraintValue += Constraint(vehicleSpareid, stationSpareid);
    double t1 = 0.0;
    double t2 = 0.0;
    double t3 = 0.0;
    //վ������Լ��
    PeolpleNumStation(departTime, arriveTime);
    for (int i = 1; i < vehicleNum + 1; i++) {//�����ǵȵ�һ�������ˣ���Ϊ�ᵼ�����һվ�ĵȴ���������
        for (int j = 0; j < stationNum; j++) {
            if (peopleNumP[i][j] > Cmax[j]) {
                t3 += 1e100 * (peopleNumP[i][j] - Cmax[j]);
            }
        }
    }

    for (int i = 0; i < vehicleNum + 1; i++) {
        for (int j = 0; j < stationNum - 1; j++) {
            for (int k = 0; k < stationFlowNum; k++) {
                int idk = stationFlowID[k];
                if (i == 0) {
                    if (arriveTime[i][idk] != -1) {
                        t1 += peopleNumP[i][j] * (arriveTime[i][idk] - 0) / 2
                            + peopleNumS[i][j] * (arriveTime[i][idk] - 0);
                    }
                }
                else {
                    double tempdepartTime = Inspect(departTime, i - 1, idk, -1);
                    double temparriveTime = Inspect(arriveTime, i, idk, 1);
                    if (tempdepartTime == -1) {
                        tempdepartTime = 0;
                    }
                    if (temparriveTime != -1) {
                        t1 += peopleNumP[i][j] * (temparriveTime - tempdepartTime) / 2
                            + peopleNumS[i][j] * (temparriveTime - tempdepartTime);
                    }
                }
                //����t3
                if (j == idk) {
                    if (i == 0) {
                        if (arriveTime[i][j] != -1) {
                            t3 += peopleNumPP[i][j] * (arriveTime[i][j] -
                                IntegralSolve(PeopleFlow, 0, arriveTime[i][j], integralNum, j, alphaC * Cmax[j])) / 2;
                        }
                    }
                    else {
                        double tempdepartTime = Inspect(departTime, i - 1, j, -1);
                        double temparriveTime = Inspect(arriveTime, i, j, 1);
                        if (tempdepartTime != -1) {
                            t3 += peopleNumPP[i][j] * (temparriveTime -
                                IntegralSolve(PeopleFlow, tempdepartTime, temparriveTime, integralNum, j, alphaC * Cmax[j])) / 2;
                        }
                    }
                }
            }
        }
    }
    for (int i = 0; i < vehicleNum + 1; i++) {
        for (int j = 0; j < stationNum; j++) {
            if (i < vehicleSpareid) {
                t2 += fabs(arriveTime[i][j] - arriveTimePre[i][j])
                    + fabs(departTime[i][j] - departTimePre[i][j]);
            }
            else if (i > vehicleSpareid && i > 0) {
                t2 += fabs(arriveTime[i][j] - arriveTimePre[i - 1][j])
                    + fabs(departTime[i][j] - departTimePre[i - 1][j]);
            }
        }
    }

    double tSum_functemp[maxLimit] = { 0 };
    for (int j = 0; j < stationNum; j++) {
        for (int jj = 0; jj < stationFlowNum; jj++) {
            if (j == stationFlowID[jj]) {
                tSum_functemp[j] = alpha * t1 + beta * t2 + gamma * t3;
            }
        }
    }
    double temptSum = 0.0;
    for (int i = 0; i < stationNum; i++) {
        if (tSum_functemp[i] > temptSum) {
            temptSum = tSum_functemp[i];
        }
    }
    return temptSum + constraintValue;
}

int* returnColumnSizes;

void permute(int* nums, int numsSize, int start, int* temp, int tempSize, int** result, int* resultSize) {
    if (tempSize > 0) {
        returnColumnSizes[*resultSize] = tempSize;
        result[*resultSize] = (int*)malloc(tempSize * sizeof(int));
        if (result[*resultSize] == NULL) {
            printf("Memory allocation failed\n");
            exit(1);
        }
        for (int i = 0; i < tempSize; i++) {
            result[*resultSize][i] = temp[i];
        }
        (*resultSize)++;
    }
    for (int i = start; i < numsSize; i++) {
        temp[tempSize] = nums[i];
        permute(nums, numsSize, i + 1, temp, tempSize + 1, result, resultSize);
    }
}

int** subsets(int* nums, int numsSize, int* returnSize) {
    int total = 1 << numsSize;
    int** result = (int**)malloc(total * sizeof(int*));
    if (result == NULL) {
        printf("Memory allocation failed\n");
        exit(1);
    }
    *returnSize = total;
    returnColumnSizes = (int*)malloc(total * sizeof(int));
    if (returnColumnSizes == NULL) {
        printf("Memory allocation failed\n");
        exit(1);
    }
    int* temp = (int*)malloc(numsSize * sizeof(int));
    if (temp == NULL) {
        printf("Memory allocation failed\n");
        exit(1);
    }
    int resultSize = 0;
    permute(nums, numsSize, 0, temp, 0, result, &resultSize);
    free(temp);
    return result;
}

void Solve() {
    double tSum_i1 = 1e100;
    for (int i_1 = 0; i_1 < stationSpareNum; i_1++) {
        if (stationSpareID[i_1] <= stationFlowID[stationFlowNum - 1]) {//��֤Ͷ�ų�վ�����һ���������վ֮ǰ
            int stationSpareid = stationSpareID[i_1];//ѭ����ȷ����Ͷ�ų�վ��id
            double dwellOut = 0;//��ͣ��ʱ������
            double dwellSpareOut = 0;//��ͣ��ʱ������
            for (int i_2 = stationSpareid; i_2 < stationFlowID[0]; i_2++) {
                dwellOut += dwellTime[i_2];
            }
            for (int i_2 = stationFlowID[0]; i_2 < stationNum; i_2++) {
                dwellSpareOut += dwellSpareTime[i_2] - dwellTime[i_2];
            }
            double tSum_i2 = 1e100;
            int vehicleSpareID = 0;
            int iterSum = 0;
            int iterbool = 1;
            for (int i_2 = 0; i_2 < vehicleNum + 1; i_2++) {//i_2Ϊ��������ǰ����
                int iter = 0;
                int status;
                const int n = vehicleNum + 1 - i_2;
                double acc = 0.01;//��������
                double tolerance = 1e-4;//��������
                int iterMax = 1000;//����������

                const gsl_multimin_fminimizer_type* T = gsl_multimin_fminimizer_nmsimplex;//Nelder-Mead�㷨
                gsl_multimin_fminimizer* s = gsl_multimin_fminimizer_alloc(T, n);
                gsl_vector* x = gsl_vector_alloc(n);//Ͷ�ų���Ͷ�ų�֮��ĳ�������վ��ʱ��
                gsl_vector* accn = gsl_vector_alloc(n);

                double headwayTOut = 0;
                double preValue = 0;
                for (int i_3 = 0; i_3 < n; i_3++) {
                    if (i_3 == 0 && i_2 != 0 && i_2 != vehicleNum) {//Ͷ�ų�
                        double time = Inspect(arriveTimePre, i_2 + i_3, stationSpareid, -1);
                        if (time == -1) {
                            time = 0;
                        }
                        gsl_vector_set(x, i_3, time + dwellOut + headwayTOut);
                    }
                    else if (i_3 == 0 && i_2 == 0) {
                        gsl_vector_set(x, i_3, 0);
                    }
                    else if (i_3 == 0 && i_2 == vehicleNum) {
                        double time = Inspect(arriveTimePre, i_2 + i_3 - 1, stationSpareid, -1);
                        if (time == -1) {
                            time = 0;
                        }
                        gsl_vector_set(x, i_3, time + dwellOut + headwayT + headwayTOut);
                    }
                    else if (i_3 != 0 && i_3 == n - 1) {//���һ����
                        double time = Inspect(arriveTimePre, i_2 + i_3 - 1, 0, -1);
                        if (time == -1) {
                            time = 0;
                        }
                        if (preValue - time < headwayT + dwellTime[0]) {
                            headwayTOut += headwayT + dwellTime[0];
                        }
                        gsl_vector_set(x, i_3, time + dwellOut + dwellSpareOut + headwayT + headwayTOut);
                    }
                    else {
                        double time = Inspect(arriveTimePre, i_2 + i_3, 0, -1);
                        if (time == -1) {
                            time = 0;
                        }
                        if (preValue - time < headwayT + dwellTime[0]) {
                            headwayTOut += headwayT + dwellTime[0];
                        }
                        gsl_vector_set(x, i_3, time + dwellOut + dwellSpareOut + headwayTOut);
                    }
                    gsl_vector_set(accn, i_3, acc);
                }
                gsl_multimin_function func;
                func.n = n;
                func.f = Function_tsum;
                int params[2] = { stationSpareid, i_2 };
                func.params = params;
                gsl_multimin_fminimizer_set(s, &func, x, accn);

                do {
                    iter++;
                    if (iter >= iterMax) {
                        iterbool = 0;
                    }
                    status = gsl_multimin_fminimizer_iterate(s);
                    if (status) break;
                    double size = gsl_multimin_fminimizer_size(s);
                    status = gsl_multimin_test_size(size, tolerance);
                } while (status == GSL_CONTINUE && iter < iterMax);

                //for (int i_3 = 0; i_3 < n; i_3++) {
                //    if (i_3 == 0) {
                //        arriveTime[i_2 + i_3][stationSpareid] = gsl_vector_get(s->x, i_3);
                //    }
                //    else {
                //        arriveTime[i_2 + i_3][0] = gsl_vector_get(s->x, i_3);
                //    }
                //}

                double tSum_i2temp = Function_tsum(s->x, params);
                if (tSum_i2temp < tSum_i2) {
                    tSum_i2 = tSum_i2temp;
                    vehicleSpareID = i_2;
                    for (int i = 0; i < vehicleNum + 1; i++) {
                        for (int j = 0; j < stationNum; j++) {
                            arriveTimeResult[stationSpareid][i][j] = arriveTime[i][j];
                            departTimeResult[stationSpareid][i][j] = departTime[i][j];
                        }
                    }
                }
                iterSum += iter;

                gsl_vector_free(x);
                gsl_multimin_fminimizer_free(s);
            }
            tSum[stationSpareid] = tSum_i2;
            printf("Ͷ�ű��ó���վ:%d%s ���ó�id:%d%s\n", stationSpareid, stationSpared, vehicleSpareID, vehicleSpared);
            if (iterbool == 0) {
                printf("���ڵ���δ���������(������û���ҵ��ȳ�ʼֵ���ŵĽ�),��ʼ����ֵΪ:% .2f\n", tSum_i2);
            }
            else if (iterbool == 1) {
                printf("Ŀ�꺯��:%.2f ÿ�ε�������������ƽ����������:%d\n",
                    tSum[stationSpareid], iterSum / (vehicleNum + 1));
            }
            {
                printf("����ʱ��:(-1��ʾ��������վ)\n");
                for (int i = 0; i < vehicleNum + 1; i++) {
                    for (int j = 0; j < stationNum; j++) {
                        printf("%.2f\t", arriveTimeResult[stationSpareid][i][j]);
                    }
                    printf("\n");
                }
                printf("��ȥʱ��:\n");
                for (int i = 0; i < vehicleNum + 1; i++) {
                    for (int j = 0; j < stationNum; j++) {
                        printf("%.2f\t", departTimeResult[stationSpareid][i][j]);
                    }
                    printf("\n");
                }
                PeolpleNumStation(departTimeResult[stationSpareid], arriveTimeResult[stationSpareid]);
                printf("��վ������һ���г���վǰ�ĵȴ��˿���:\n");
                for (int i = 0; i < vehicleNum + 1; i++) {
                    for (int j = 0; j < stationNum; j++) {
                        printf("%.0f\t", peopleNumP[i][j]);
                    }
                    printf("\n");
                }
                printf("\n");
            }

            if (tSum_i2 < tSum_i1) {
                tSum_i1 = tSum_i2;
                stationSpareidEvery = stationSpareid;
                vehicleSpareidEvery = vehicleSpareID;
                for (int i = 0; i < vehicleNum + 1; i++) {
                    for (int j = 0; j < stationNum; j++) {
                        arriveTimeResultTemp[i][j] = arriveTimeResult[stationSpareid][i][j];
                        departTimeResultTemp[i][j] = departTimeResult[stationSpareid][i][j];
                    }
                }
            }
        }
    }
}

/*
* main������ö���˱��ó����ĸ�վ��Ͷ�š������������г���ͬ����Ŀ��ܲ�����Щ�������Сt�ķ�����
* ��ÿ�������ʹ�ö�Ԫ����������С�����㷨���е�����Ŀ����������ʱ�����������ֵ��
* gsl_vector_set����Ϊ���ó��Լ����ó����ÿһ�����ķ���ʱ�䣨ͬ����Ҫ���ı��������ֵ��
* ��ֵӦ��֤�ڿ������ڣ�ԭ������ȥʱ��Ĳ���ȷ��ht��hs�Ĺ����ϸ���ᵼ�³�ʼ��ʧЧ
*/
int main() {

    for (int i = 0; i < maxLimit; i++) {
        Cmax[i] = 10000;
        for (int j = 0; j < maxLimit; j++) {
            changeable[i][j] = 1;
        }
    }

    stationFlowNum = sizeof(stationFlowID) / sizeof(stationFlowID[0]);
    stationSpareNum = sizeof(stationSpareID) / sizeof(stationSpareID[0]);

    //int returnSize;
    //int** stationSpareIDCombine = subsets(stationSpareID, stationSpareNum, &returnSize);
    //��֤Ͷ�ų�վ�����д������վ֮ǰ?undo
    
    for (int i = 0; i < 1; i++) {
        Solve();
        for (int i = 0; i < vehicleNum + 1; i++) {
            for (int j = 0; j < stationNum; j++) {
                arriveTimePre[i][j] = arriveTimeResultTemp[i][j];
                departTimePre[i][j] = arriveTimeResultTemp[i][j];
            }
        }
        char idString[10];
        sprintf_s(idString, sizeof(idString), "%d", stationSpareidEvery);
        strcat_s(stationSpared, sizeof(stationSpared), ",");
        strcat_s(stationSpared, sizeof(stationSpared), idString);
        sprintf_s(idString, sizeof(idString), "%d", vehicleSpareidEvery);
        strcat_s(vehicleSpared, sizeof(vehicleSpared), ",");
        strcat_s(vehicleSpared, sizeof(vehicleSpared), idString);
        vehicleNum++;
        depth++;
    }
    return 0;
}
