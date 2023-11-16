#include <stdio.h>
#include <stdlib.h>
#include <gsl/gsl_multimin.h>

#define maxLimit 20

/*
* ���б����г���tSum��tSumMax��stationFlowNum��stationSpareNum��beta��
* departTimeResult��arriveTimeResult���ǿɱ༭���ݼ�ǰ�����
*/
int vehicleNum = 2; // ������
int stationNum = 13; // ��վ��
double departTime[maxLimit][maxLimit];//��վʱ��
double arriveTime[maxLimit][maxLimit];//��վʱ��
double departTimePre[maxLimit][maxLimit] = {//��Ϊ�г�����Ϊվ�㣬Ԫ��Ϊʱ��
    { 60, 160, 260, 360, 460, 560, 660, 760, 860, 960, 1060, 1160, 1260 },
    { 360, 460, 560, 660, 760, 860, 960, 1060, 1160, 1260, 1360, 1460, 1560 },
};//ԭ��վʱ��
double arriveTimePre[maxLimit][maxLimit] = {
    { 30, 130, 230, 330, 430, 530, 630, 730, 830, 930, 1030, 1130, 1230 },
    { 330, 430, 530, 630, 730, 830, 930, 1030, 1130, 1230, 1330, 1430, 1530 },
};//ԭ��վʱ��
double dwellTime[maxLimit] = {
    30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30
};//ͣվʱ��
double dwellSpareTime[maxLimit] = {
    30, 30, 30, 30, 30, 30, 70, 30, 30, 30, 30, 30, 30
};//���ó�ͣվʱ��
double runTime[maxLimit] = {
    70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70
};//����һվ������ʱ��
double headwayT = 180;//ht
double headwayS = 600;//hs
double tSum[maxLimit];
double tSumMax;
double peopleNumP[maxLimit][maxLimit];//�ȴ��˿���
double peopleNumS[maxLimit][maxLimit];//�����˿���
int stationFlowID[2] = { 6, 12 };//�������վid����˳��д
int stationFlowNum;//�������վ����
int stationSpareID[3] = { 0, 5, 7 };//���ó���Ͷ�ų�վID����˳��д
int stationSpareNum;//���ó���Ͷ�ų�վ����
float alpha = 0.5;
float beta;
double departTimeResult[maxLimit][maxLimit][maxLimit];
double arriveTimeResult[maxLimit][maxLimit][maxLimit];

double PeopleFlow(double t, int idStation) {//������������Ϊÿ��վ������Լ��Ĺ���t��һԪ����
    return 1.0 / 10.0;
}

double Integral(double (*f)(double, int), double a, double b, double n, int idStation) {//�����
    double h = (b - a) / n;
    double result = 0.0;
    for (int i = 0; i < n; i++) {
        result += (f(((double)i + 1.0) * h, idStation) + f((double)i * h, idStation)) / 2 * h;
    }
    return result;
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

int Constraint(int vehicleSpareid, int stationSpareid) {//����������
    for (int i = 0; i < vehicleNum + 1; i++) {
        if (i != 0) {
            for (int j = 0; j < stationNum; j++) {
                if (j < stationSpareid) {
                    if (i != vehicleSpareid && i - 1 != vehicleSpareid) {
                        if (arriveTime[i][j] - arriveTime[i - 1][j] < headwayT) {
                            return 0;
                        }
                    }
                    if (i == vehicleSpareid) {
                        if (arriveTime[i + 1][j] - arriveTime[i - 1][j] > 1.5 * headwayS) {
                            return 0;
                        }
                    }
                }
                else {
                    if (arriveTime[i][j] - arriveTime[i - 1][j] < headwayT) {
                        return 0;
                    }
                }
            }
            if (i == vehicleSpareid) {
                if (arriveTime[i][stationSpareid] < departTime[i - 1][stationSpareid])
                {
                    return 0;
                }
            }
            else {
                if (arriveTime[i][0] < departTime[i - 1][0])
                {
                    return 0;
                }
            }
        }
        if (i == vehicleSpareid) {
            if (arriveTime[i][stationSpareid] < 0)
            {
                return 0;
            }
        }
        else {
            if (arriveTime[i][0] < 0)
            {
                return 0;
            }
        }
    }
    return 1;
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
    if (Constraint(vehicleSpareid, stationSpareid) == 0) {
        return 1e100;
    }
    else {
        double t1 = 0.0;
        double t2 = 0.0;
        for (int i = 0; i < vehicleNum + 1; i++) {//����һ�����ó�
            for (int j = 0; j < stationNum; j++) {
                if (i == 0) {
                    if (departTime[i][j] != -1) {
                        peopleNumP[i][j] = Integral(PeopleFlow, 0.0, departTime[i][j], 1000.0, j);
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
                        double bTime = Inspect(departTime, i, j, 1);
                        if (aTime == -1) {
                            peopleNumP[i][j] = Integral(PeopleFlow, 0, bTime, 1000.0, j);
                        }
                        else {
                            peopleNumP[i][j] = Integral(PeopleFlow, aTime, bTime, 1000.0, j);
                        }
                    }
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
        double tSum_functemp[maxLimit] = {0};
        for (int j = 0; j < stationNum; j++) {
            for (int jj = 0; jj < stationFlowNum; jj++) {
                if (j == stationFlowID[jj]) {
                    tSum_functemp[j] = alpha * t1 + beta * t2;
                }
            }
        }
        double temptSum = 0.0;
        for (int i = 0; i < stationNum; i++) {
            if (tSum_functemp[i] > temptSum) {
                temptSum = tSum_functemp[i];
            }
        }
        return temptSum;
    }
}

/*
* main������ö���˱��ó����ĸ�վ��Ͷ�š������������г���ͬ����Ŀ��ܲ�����Щ�������Сt�ķ�����
* ��ÿ�������ʹ�ö�Ԫ����������С�����㷨���е�����Ŀ����������ʱ�����������ֵ��
* gsl_vector_set����Ϊ���ó��Լ����ó����ÿһ�����ķ���ʱ�䣨ͬ����Ҫ���ı��������ֵ��
* ��ֵӦ��֤�ڿ������ڣ�ԭ������ȥʱ��Ĳ���ȷ��ht��hs�Ĺ����ϸ���ᵼ�³�ʼ��ʧЧ
*/
int main() {
    stationFlowNum = sizeof(stationFlowID) / sizeof(stationFlowID[0]);
    stationSpareNum = sizeof(stationSpareID) / sizeof(stationSpareID[0]);
    beta = 1 - alpha;

    for (int i_1 = 0; i_1 < stationSpareNum; i_1++) {
        if (stationSpareID[i_1] <= stationFlowID[0]) {//��֤Ͷ�ų�վ�����д������վ֮ǰ
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
                for (int i_3 = 0; i_3 < n; i_3++) {
                    if (i_3 == 0 && i_2 != 0 && i_2 != vehicleNum) {//Ͷ�ų�
                        gsl_vector_set(x, i_3, arriveTimePre[i_2 + i_3][stationSpareid] + dwellOut);
                    }
                    else if (i_3 == 0 && i_2 == 0) {
                        gsl_vector_set(x, i_3, 0);
                    }
                    else if (i_3 == 0 && i_2 == vehicleNum) {
                        gsl_vector_set(x, i_3, arriveTimePre[i_2 + i_3 - 1][stationSpareid] + dwellOut + headwayT);
                    }
                    else if (i_3 != 0 && i_3 == n - 1) {//���һ����
                        gsl_vector_set(x, i_3, arriveTimePre[i_2 + i_3 - 1][0] + dwellOut + dwellSpareOut + headwayT);
                    }
                    else {
                        gsl_vector_set(x, i_3, arriveTimePre[i_2 + i_3][0] + dwellOut + dwellSpareOut);
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
            printf("Ͷ�ű��ó���վ��%d ���ó�id��%d\n", stationSpareid, vehicleSpareID);
            if (iterbool == 1) {
                printf("Ŀ�꺯����%.2f ÿ�ε�������������ƽ������������%d\n", 
                    tSum[stationSpareid], iterSum / (vehicleNum + 1));
                printf("����ʱ�䣺��-1��ʾ��������վ��\n");
                for (int i = 0; i < vehicleNum + 1; i++) {
                    for (int j = 0; j < stationNum; j++) {
                        printf("%.2f\t", arriveTimeResult[stationSpareid][i][j]);
                    }
                    printf("\n");
                }
                printf("��ȥʱ�䣺\n");
                for (int i = 0; i < vehicleNum + 1; i++) {
                    for (int j = 0; j < stationNum; j++) {
                        printf("%.2f\t", departTimeResult[stationSpareid][i][j]);
                    }
                    printf("\n");
                }
                printf("\n");
            }
            else {
                printf("���ڵ���δ���������\n");
            }
        }
    }
    
    return 0;
}
