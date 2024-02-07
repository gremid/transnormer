FROM gremid/cuda-clojure-python:8916533 AS builder

RUN pip install --no-cache-dir transformers[torch]

RUN mkdir -p /build

WORKDIR /build

COPY deps.edn /build/

RUN clojure -P && \
    clojure -A:build -P

COPY ./ /build

RUN clojure -T:build jar

FROM gremid/cuda-clojure-python:8916533

RUN pip install --no-cache-dir transformers[torch]

RUN mkdir -p /service
WORKDIR /service

ENV JDK_JAVA_OPTIONS "-XX:+UseContainerSupport -XX:MaxRAMPercentage=75 --enable-native-access=ALL-UNNAMED"

COPY --from=builder /build/transnormer.jar /service

ENTRYPOINT ["java", "-jar", "/service/transnormer.jar"]

